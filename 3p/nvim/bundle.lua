#!/usr/bin/env lua
-- Bundles pre-fetched plugins into an nvim directory
-- Usage: lua lib/build/nvim-bundle.lua <platform> <nvim_dir> <plugins_dir>

local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("spawn").spawn

local PACK_LOCK = ".config/nvim/nvim-pack-lock.json"

local function execute(program, args, opts)
  opts = opts or {}
  local handle, err = spawn(args)
  if not handle then
    if opts.allow_failure then return false end
    return nil, string.format("command failed to start: %s (%s)", program, err or "unknown error")
  end
  local exit_code, wait_err = handle:wait()
  if not exit_code then
    if opts.allow_failure then return false end
    return nil, string.format("command failed: %s (%s)", program, wait_err or "abnormal termination")
  end
  if exit_code ~= 0 then
    if opts.allow_failure then return false end
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

local function list_plugins(content)
  local plugins = {}
  local in_plugins = false

  for line in content:gmatch("[^\n]+") do
    if line:match('^%s*"plugins":%s*{%s*$') then
      in_plugins = true
    elseif in_plugins then
      local name = line:match('^%s*"([^"]+)":%s*{%s*$')
      if name then
        table.insert(plugins, name)
      end
    end
  end

  return plugins
end

local function fetch_plugin_inline(plugin_name, output_dir, pack_lock_content)
  local info = {}
  local in_plugin = false
  for line in pack_lock_content:gmatch("[^\n]+") do
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
    return nil, "plugin not found: " .. plugin_name
  end

  local owner, repo = info.src:match("github%.com/([^/]+)/([^/]+)$")
  if not owner or not repo then
    return nil, "invalid GitHub URL: " .. info.src
  end

  io.write(string.format("  fetching %s at %s\n", plugin_name, info.rev))
  local url = string.format("https://github.com/%s/%s/archive/%s.tar.gz", owner, repo, info.rev)
  local tarball = output_dir .. ".tar.gz"

  -- ensure parent directory exists
  local parent = output_dir:match("(.+)/[^/]+$")
  if parent then
    unix.makedirs(parent)
  end

  local cosmo = require("cosmo")
  local status, headers, body
  local fetch_opts = {headers = {["User-Agent"] = "curl/8.0"}, maxresponse = 300 * 1024 * 1024}
  for attempt = 1, 8 do
    status, headers, body = cosmo.Fetch(url, fetch_opts)
    if status then break end
    if attempt < 8 then
      unix.nanosleep(math.min(30, 2 ^ attempt), 0)
    end
  end
  if not status or status ~= 200 then
    return nil, "fetch failed for " .. plugin_name
  end

  local fd, err = unix.open(tarball, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("0644", 8))
  if not fd or fd < 0 then return nil, "failed to create tarball at " .. tarball .. ": " .. tostring(err) end
  unix.write(fd, body)
  unix.close(fd)

  unix.makedirs(output_dir)
  local ok, err = execute("tar", {"tar", "-xzf", tarball, "-C", output_dir, "--strip-components=1"})
  unix.unlink(tarball)
  if not ok then return nil, err end
  return true
end

local function bundle_plugins(nvim_dir, plugins_dir, plugins, pack_lock_content)
  io.write("bundling plugins\n")

  local pack_dir = path.join(nvim_dir, "share/nvim/site/pack/core/opt")
  local ok, err = execute("mkdir", {"mkdir", "-p", pack_dir})
  if not ok then
    return nil, err
  end

  for _, name in ipairs(plugins) do
    local src = path.join(plugins_dir, name)
    local dst = path.join(pack_dir, name)

    -- fetch if missing
    local st = unix.stat(src)
    if not st then
      unix.makedirs(plugins_dir)
      ok, err = fetch_plugin_inline(name, src, pack_lock_content)
      if not ok then
        return nil, err
      end
    end

    io.write(string.format("  copying %s\n", name))
    local cp_ok, cp_err = execute("cp", {"cp", "-r", src, dst})
    if not cp_ok then
      return nil, string.format("failed to copy %s: %s", name, cp_err)
    end
  end

  io.write("generating helptags\n")
  local nvim_bin = path.join(nvim_dir, "bin/nvim")
  ok = execute(nvim_bin, {nvim_bin, "--headless", "+helptags ALL", "+qa"}, { allow_failure = true })
  if not ok then
    io.write("  helptags skipped (cross-platform build)\n")
  end

  return true
end

local function install_treesitter_parsers(nvim_dir)
  io.write("installing treesitter parsers\n")
  local nvim_bin = path.join(nvim_dir, "bin/nvim")

  local first_ok = execute(nvim_bin, {nvim_bin, "--version"}, { allow_failure = true })
  if not first_ok then
    io.write("  parsers skipped (cross-platform build)\n")
    return true
  end

  local cwd = unix.getcwd()
  local config_home = path.join(cwd, ".config")
  local data_home = path.join(nvim_dir, "share")

  local parsers = dofile(path.join(cwd, ".config/nvim/parsers.lua"))
  for _, parser in ipairs(parsers) do
    io.write(string.format("  installing %s\n", parser))
    local cmd = string.format("TSUpdateSync %s", parser)
    local ok = execute("env", {
      "env",
      "XDG_CONFIG_HOME=" .. config_home,
      "XDG_DATA_HOME=" .. data_home,
      nvim_bin, "--headless", "+" .. cmd, "+qa"
    }, { allow_failure = true })
    if not ok then
      io.write(string.format("    %s failed, continuing\n", parser))
    end
  end

  return true
end

local function verify_plugins(nvim_dir, plugins)
  io.write("verifying plugins\n")
  local nvim_bin = path.join(nvim_dir, "bin/nvim")

  local first_ok = execute(nvim_bin, {nvim_bin, "--version"}, { allow_failure = true })
  if not first_ok then
    io.write("  verification skipped (cross-platform build)\n")
    return true
  end

  for _, name in ipairs(plugins) do
    local cmd = string.format("+packadd %s", name)
    local ok, err = execute(nvim_bin, {nvim_bin, "--headless", cmd, "+qa"})
    if not ok then
      return nil, string.format("plugin %s failed to load: %s", name, err)
    end
  end

  return true
end

local function bundle(platform, nvim_dir, plugins_dir)
  if not platform or platform == "" then
    return nil, "platform is required"
  end
  if not nvim_dir or nvim_dir == "" then
    return nil, "nvim_dir is required"
  end
  if not plugins_dir or plugins_dir == "" then
    return nil, "plugins_dir is required"
  end

  nvim_dir = nvim_dir:gsub("/$", "")
  plugins_dir = plugins_dir:gsub("/$", "")

  unix.makedirs(plugins_dir)

  unix.unveil(PACK_LOCK, "r")
  unix.unveil(".config/nvim", "r")
  unix.unveil(plugins_dir, "rwc")
  unix.unveil(nvim_dir, "rwcx")
  unix.unveil("/etc/resolv.conf", "r")
  unix.unveil("/etc/ssl", "r")
  unix.unveil("/usr", "rx")
  unix.unveil(nil, nil)

  local content, err = read_file(PACK_LOCK)
  if not content then
    return nil, err
  end

  local plugins = list_plugins(content)
  if #plugins == 0 then
    return nil, "no plugins found in pack-lock"
  end

  local ok
  ok, err = bundle_plugins(nvim_dir, plugins_dir, plugins, content)
  if not ok then
    return nil, err
  end

  install_treesitter_parsers(nvim_dir)

  ok, err = verify_plugins(nvim_dir, plugins)
  if not ok then
    return nil, err
  end

  io.write("bundle complete\n")
  return true
end

if not pcall(debug.getlocal, 4, 1) then
  local platform = arg[1]
  local nvim_dir = arg[2]
  local plugins_dir = arg[3]

  local ok, err = bundle(platform, nvim_dir, plugins_dir)
  if not ok then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
  os.exit(0)
end

return { bundle = bundle }
