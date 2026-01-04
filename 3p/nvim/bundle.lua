#!/usr/bin/env lua
-- Bundles pre-fetched plugins into an nvim directory
-- Usage: lua lib/build/nvim-bundle.lua <platform> <nvim_dir> <plugins_dir>

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")

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

local function parse_pack_lock(content)
  local data = cosmo.DecodeJson(content)
  if not data or not data.plugins then
    return nil, "invalid pack-lock format"
  end
  return data
end

local function list_plugins(data)
  local plugins = {}
  for name, _ in pairs(data.plugins) do
    table.insert(plugins, name)
  end
  table.sort(plugins)
  return plugins
end

local function fetch_plugin_inline(plugin_name, output_dir, pack_lock_data)
  local info = pack_lock_data.plugins[plugin_name]
  if not info or not info.src or not info.rev then
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
  unix.makedirs(path.dirname(output_dir))

  local status, _, body
  local fetch_opts = {headers = {["User-Agent"] = "curl/8.0"}, maxresponse = 300 * 1024 * 1024}
  for attempt = 1, 8 do
    status, _, body = cosmo.Fetch(url, fetch_opts)
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
  local ok, extract_err = execute("tar", {"tar", "-xzf", tarball, "-C", output_dir, "--strip-components=1"})
  unix.unlink(tarball)
  if not ok then return nil, extract_err end
  return true
end

local function bundle_plugins(nvim_dir, plugins_dir, plugins, pack_lock_data)
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
      ok, err = fetch_plugin_inline(name, src, pack_lock_data)
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

local function install_treesitter_parsers(nvim_dir, site_dir)
  io.write("installing treesitter parsers\n")
  local nvim_bin = path.join(nvim_dir, "bin/nvim")

  local first_ok = execute(nvim_bin, {nvim_bin, "--version"}, { allow_failure = true })
  if not first_ok then
    io.write("  parsers skipped (cross-platform build)\n")
    return true
  end

  local cwd = unix.getcwd()
  local cache_dir = path.join(cwd, "o/any/nvim/cache")
  execute("mkdir", {"mkdir", "-p", cache_dir})

  local parsers = dofile(path.join(cwd, ".config/nvim/parsers.lua"))
  local parser_list = '"' .. table.concat(parsers, '","') .. '"'

  -- use absolute paths (nvim may run from different cwd)
  local abs_site_dir = path.join(cwd, site_dir)

  -- write lua script to temp file (avoids shell escaping issues)
  local lua_script = string.format([[
vim.opt.packpath:prepend("%s")
vim.cmd("packadd nvim-treesitter")
local ts = require("nvim-treesitter")
ts.setup({ install_dir = "%s" })
local ok = ts.install({%s}):wait()
if ok then vim.cmd("qall!") else vim.cmd("cquit 1") end
]], abs_site_dir, abs_site_dir, parser_list)

  local script_path = path.join(cache_dir, "install_parsers.lua")
  local fd, err = unix.open(script_path, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("0644", 8))
  if not fd or fd < 0 then
    io.write(string.format("  failed to write install script at %s: %s\n", script_path, tostring(err)))
    return true
  end
  unix.write(fd, lua_script)
  unix.close(fd)

  io.write(string.format("  installing: %s\n", table.concat(parsers, ", ")))
  local cmd = { nvim_bin, "--headless", "-u", "NONE", "-l", script_path }
  local abs_nvim_dir = path.join(cwd, nvim_dir)
  local nvim_runtime = path.join(abs_nvim_dir, "share/nvim/runtime")
  local env = unix.environ()
  env.XDG_CACHE_HOME = cache_dir
  env.VIMRUNTIME = nvim_runtime
  env.VIM = path.join(abs_nvim_dir, "share/nvim")
  local handle = spawn(cmd, { env = env })
  local stderr_content = handle.stderr:read()
  local ok, output, exit_code = handle:read()
  if output and output ~= "" then
    for line in output:gmatch("[^\n]+") do
      io.write("  " .. line .. "\n")
    end
  end
  if not ok then
    if stderr_content and stderr_content ~= "" then
      for line in stderr_content:gmatch("[^\n]+") do
        io.write("  stderr: " .. line .. "\n")
      end
    end
    io.write(string.format("  parser installation failed (exit %s)\n", tostring(exit_code)))
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

  local cwd = unix.getcwd()
  local cache_dir = path.join(cwd, "o/any/nvim/cache")
  unix.makedirs(cache_dir)

  -- TODO: re-enable unveil sandbox once treesitter parser installation works
  -- The sandbox blocks tree-sitter CLI and compiler access needed for parser builds
  -- local home = os.getenv("HOME") or "/tmp"
  -- unix.unveil(PACK_LOCK, "r")
  -- unix.unveil(".config/nvim", "r")
  -- unix.unveil(plugins_dir, "rwc")
  -- unix.unveil(nvim_dir, "rwcx")
  -- unix.unveil(cache_dir, "rwc")
  -- unix.unveil(cwd, "rwc")
  -- unix.unveil("/tmp", "rwc")
  -- unix.unveil("/etc/resolv.conf", "r")
  -- unix.unveil("/etc/ssl", "r")
  -- unix.unveil("/usr", "rx")
  -- unix.unveil(path.join(home, ".local/share"), "rx")
  -- unix.unveil(nil, nil)

  local content, err = read_file(PACK_LOCK)
  if not content then
    return nil, err
  end

  local pack_lock_data
  pack_lock_data, err = parse_pack_lock(content)
  if not pack_lock_data then
    return nil, err
  end

  local plugins = list_plugins(pack_lock_data)
  if #plugins == 0 then
    return nil, "no plugins found in pack-lock"
  end

  local ok
  ok, err = bundle_plugins(nvim_dir, plugins_dir, plugins, pack_lock_data)
  if not ok then
    return nil, err
  end

  local site_dir = path.join(nvim_dir, "share/nvim/site")
  install_treesitter_parsers(nvim_dir, site_dir)

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
