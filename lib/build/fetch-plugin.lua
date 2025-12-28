#!/usr/bin/env lua
-- Fetches a single nvim plugin from GitHub archive
-- Usage: lua lib/build/fetch-plugin.lua <plugin-name> <output-dir>
-- Reads plugin info from .config/nvim/nvim-pack-lock.json

local cosmo = require("cosmo")
local unix = cosmo.unix
local spawn = require("spawn").spawn

local PACK_LOCK = ".config/nvim/nvim-pack-lock.json"

local function execute(program, args)
  local handle, err = spawn(args)
  if not handle then
    return nil, string.format("command failed to start: %s (%s)", program, err or "unknown error")
  end
  local exit_code, wait_err = handle:wait()
  if not exit_code then
    return nil, string.format("command failed: %s (%s)", program, wait_err or "abnormal termination")
  end
  if exit_code ~= 0 then
    return nil, string.format("command failed: %s (exit: %d)", program, exit_code)
  end
  return true
end

local function read_file(filepath)
  local fd = unix.open(filepath, unix.O_RDONLY)
  if not fd then
    return nil, "failed to open " .. filepath
  end
  local chunks = {}
  while true do
    local chunk = unix.read(fd, 65536)
    if not chunk or chunk == "" then break end
    table.insert(chunks, chunk)
  end
  unix.close(fd)
  return table.concat(chunks)
end

local function parse_plugin_info(content, plugin_name)
  local in_plugin = false
  local info = {}

  for line in content:gmatch("[^\n]+") do
    local name = line:match('^%s*"([^"]+)":%s*{%s*$')
    if name == plugin_name then
      in_plugin = true
    elseif in_plugin then
      local key, value = line:match('^%s*"([^"]+)":%s*"([^"]*)"')
      if key then
        info[key] = value
      end
      if line:match("^%s*}") then
        break
      end
    end
  end

  if not info.src or not info.rev then
    return nil, "plugin not found or missing src/rev: " .. plugin_name
  end
  return info
end

local function fetch_plugin(plugin_name, output_dir)
  if not plugin_name or plugin_name == "" then
    return nil, "plugin name is required"
  end
  if not output_dir or output_dir == "" then
    return nil, "output directory is required"
  end

  output_dir = output_dir:gsub("/$", "")

  local content, err = read_file(PACK_LOCK)
  if not content then
    return nil, err
  end

  local info
  info, err = parse_plugin_info(content, plugin_name)
  if not info then
    return nil, err
  end

  io.write(string.format("fetching %s at %s\n", plugin_name, info.rev))

  local owner, repo = info.src:match("github%.com/([^/]+)/([^/]+)$")
  if not owner or not repo then
    return nil, "invalid GitHub URL: " .. info.src
  end

  local url = string.format("https://github.com/%s/%s/archive/%s.tar.gz", owner, repo, info.rev)
  local tarball = output_dir .. ".tar.gz"

  -- Ensure parent directory exists
  local parent = output_dir:match("(.+)/[^/]+$")
  if parent then
    unix.makedirs(parent)
  end

  -- Download with retry
  local status, headers, body
  local last_err
  local max_attempts = 8
  local fetch_opts = {headers = {["User-Agent"] = "curl/8.0"}, maxresponse = 300 * 1024 * 1024}
  for attempt = 1, max_attempts do
    status, headers, body = cosmo.Fetch(url, fetch_opts)
    if status then
      break
    end
    last_err = tostring(headers or "unknown error")
    if attempt < max_attempts then
      local delay = math.min(30, 2 ^ attempt)
      unix.nanosleep(delay, 0)
    end
  end
  if not status then
    return nil, "fetch failed: " .. last_err
  end
  if status ~= 200 then
    return nil, "fetch failed with status " .. tostring(status)
  end

  local fd = unix.open(tarball, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("0644", 8))
  if not fd or fd < 0 then
    return nil, "failed to create tarball"
  end
  unix.write(fd, body)
  unix.close(fd)

  -- Extract
  unix.makedirs(output_dir)

  local tar_ok, tar_err = execute("tar", {"tar", "-xzf", tarball, "-C", output_dir, "--strip-components=1"})
  if not tar_ok then
    unix.unlink(tarball)
    return nil, tar_err
  end

  -- Cleanup tarball
  unix.unlink(tarball)

  return true
end

if not pcall(debug.getlocal, 4, 1) then
  local plugin_name = arg[1]
  local output_dir = arg[2]

  local ok, err = fetch_plugin(plugin_name, output_dir)
  if not ok then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
  os.exit(0)
end

return { fetch_plugin = fetch_plugin }
