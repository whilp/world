#!/usr/bin/env lua
-- Bundles pre-fetched plugins into an nvim directory
-- Usage: bundle.lua <platform> <nvim_dir> <plugins_dir>

local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")

local function execute(cmd, opts)
  opts = opts or {}
  local handle, err = spawn(cmd)
  if not handle then
    if opts.allow_failure then return false end
    return nil, string.format("%s: failed to start: %s", cmd[1], err or "unknown")
  end
  local ok, _, exit_code = handle:read()
  if not ok or exit_code ~= 0 then
    if opts.allow_failure then return false end
    return nil, string.format("%s: exit %s", cmd[1], tostring(exit_code))
  end
  return true
end

local function list_dirs(dir)
  local entries = {}
  local d = unix.opendir(dir)
  if not d then return entries end
  while true do
    local name = d:read()
    if not name then break end
    if name ~= "." and name ~= ".." and not name:match("^%.") then
      local p = path.join(dir, name)
      local st = unix.stat(p)
      if st and unix.S_ISDIR(st:mode()) then
        table.insert(entries, name)
      end
    end
  end
  d:close()
  table.sort(entries)
  return entries
end

local function copy_plugins(nvim_dir, plugins_dir)
  io.write("bundling plugins\n")
  local pack_dir = path.join(nvim_dir, "share/nvim/site/pack/core/opt")
  unix.makedirs(pack_dir)

  local plugins = list_dirs(plugins_dir)
  for _, name in ipairs(plugins) do
    io.write(string.format("  copying %s\n", name))
    local src = path.join(plugins_dir, name)
    local dst = path.join(pack_dir, name)
    local ok, err = execute({"cp", "-r", src, dst})
    if not ok then
      return nil, err
    end
  end
  return plugins
end

local function generate_helptags(nvim_dir)
  io.write("generating helptags\n")
  local nvim_bin = path.join(nvim_dir, "bin/nvim")
  local ok = execute({nvim_bin, "--headless", "+helptags ALL", "+qa"}, {allow_failure = true})
  if not ok then
    io.write("  skipped (nvim not executable)\n")
  end
  return true
end

local function install_parsers(nvim_dir)
  io.write("installing treesitter parsers\n")
  local nvim_bin = path.join(nvim_dir, "bin/nvim")

  if not execute({nvim_bin, "--version"}, {allow_failure = true}) then
    io.write("  skipped (nvim not executable)\n")
    return true
  end

  local cwd = unix.getcwd()
  local parsers = dofile(path.join(cwd, ".config/nvim/parsers.lua"))
  local site_dir = path.join(cwd, nvim_dir, "share/nvim/site")

  local script = string.format([[
vim.opt.packpath:prepend("%s")
vim.cmd("packadd nvim-treesitter")
local ts = require("nvim-treesitter")
ts.setup({ install_dir = "%s" })
local ok = ts.install({"%s"}):wait()
if ok then vim.cmd("qall!") else vim.cmd("cquit 1") end
]], site_dir, site_dir, table.concat(parsers, '","'))

  local script_path = path.join(cwd, "o/nvim/install_parsers.lua")
  local fd = unix.open(script_path, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("644", 8))
  if not fd then
    return nil, "failed to write install script"
  end
  unix.write(fd, script)
  unix.close(fd)

  io.write(string.format("  installing: %s\n", table.concat(parsers, ", ")))

  local env = unix.environ()
  env.XDG_CACHE_HOME = path.join(cwd, "o/nvim/cache")
  env.VIMRUNTIME = path.join(cwd, nvim_dir, "share/nvim/runtime")
  env.VIM = path.join(cwd, nvim_dir, "share/nvim")

  local handle = spawn({nvim_bin, "--headless", "-u", "NONE", "-l", script_path}, {env = env})
  local ok, output = handle:read()
  if not ok then
    local stderr = handle.stderr:read()
    if stderr and stderr ~= "" then
      io.write("  " .. stderr:gsub("\n", "\n  ") .. "\n")
    end
    return nil, "parser installation failed"
  end
  return true
end

local function verify_plugins(nvim_dir, plugins)
  io.write("verifying plugins\n")
  local nvim_bin = path.join(nvim_dir, "bin/nvim")

  if not execute({nvim_bin, "--version"}, {allow_failure = true}) then
    io.write("  skipped (nvim not executable)\n")
    return true
  end

  for _, name in ipairs(plugins) do
    local ok, err = execute({nvim_bin, "--headless", "+packadd " .. name, "+qa"})
    if not ok then
      return nil, string.format("plugin %s failed to load: %s", name, err)
    end
  end
  return true
end

local function bundle(platform, nvim_dir, plugins_dir)
  if not nvim_dir or nvim_dir == "" then
    return nil, "nvim_dir is required"
  end
  if not plugins_dir or plugins_dir == "" then
    return nil, "plugins_dir is required"
  end

  local plugins, err = copy_plugins(nvim_dir, plugins_dir)
  if not plugins then
    return nil, err
  end

  generate_helptags(nvim_dir)

  local ok
  ok, err = install_parsers(nvim_dir)
  if not ok then
    return nil, err
  end

  ok, err = verify_plugins(nvim_dir, plugins)
  if not ok then
    return nil, err
  end

  io.write("bundle complete\n")
  return true
end

if not pcall(debug.getlocal, 4, 1) then
  local ok, err = bundle(arg[1], arg[2], arg[3])
  if not ok then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
end

return { bundle = bundle }
