#!/usr/bin/env lua
-- Bundles plugins into an extracted nvim directory
-- Usage: lua lib/build/nvim-bundle.lua <platform> <nvim_dir>
-- Reads plugin list from .config/nvim/nvim-pack-lock.json

local cosmo = require("cosmo")
local path = cosmo.path
local unix = cosmo.unix
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

local function parse_pack_lock(content)
  local plugins = {}
  local current_plugin = nil
  local current_info = {}
  local in_plugins = false

  for line in content:gmatch("[^\n]+") do
    if line:match('^%s*"plugins":%s*{%s*$') then
      in_plugins = true
    elseif in_plugins then
      local plugin_name = line:match('^%s*"([^"]+)":%s*{%s*$')
      if plugin_name then
        if current_plugin then
          plugins[current_plugin] = current_info
        end
        current_plugin = plugin_name
        current_info = {}
      end

      local key, value = line:match('^%s*"([^"]+)":%s*"([^"]*)"')
      if key and current_plugin then
        current_info[key] = value
      end

      if line:match("^%s*}") and current_plugin then
        plugins[current_plugin] = current_info
        current_plugin = nil
        current_info = {}
      end
    end
  end

  return { plugins = plugins }
end

local function load_pack_lock(lock_file)
  local content, err = read_file(lock_file)
  if not content then
    return nil, err
  end

  return parse_pack_lock(content)
end

local S_IFDIR = 16384

local function dir_exists(dir_path)
  local stat = unix.stat(dir_path)
  if not stat then return false end
  local mode = stat:mode()
  return (mode - (mode % S_IFDIR)) / S_IFDIR % 2 == 1
end

local function clone_plugin(name, info, pack_dir)
  local plugin_dir = path.join(pack_dir, name)

  if dir_exists(plugin_dir) then
    io.write(string.format("  %s already exists, skipping\n", name))
    return true
  end

  io.write(string.format("  cloning %s at %s\n", name, info.rev))

  local ok = execute("git", {
    "git", "clone", "--depth", "1", "--branch", info.rev, info.src, plugin_dir
  }, { allow_failure = true })

  if not ok then
    io.write("    branch clone failed, trying checkout\n")
    local clone_ok, clone_err = execute("git", {"git", "clone", info.src, plugin_dir})
    if not clone_ok then
      return nil, clone_err
    end
    local checkout_ok, checkout_err = execute("git", {"git", "-C", plugin_dir, "checkout", info.rev})
    if not checkout_ok then
      return nil, checkout_err
    end
  end

  local rm_ok, rm_err = execute("rm", {"rm", "-rf", path.join(plugin_dir, ".git")})
  if not rm_ok then
    return nil, rm_err
  end

  if name == "nui-components.nvim" then
    execute("rm", {"rm", "-rf", path.join(plugin_dir, "docs/public")}, { allow_failure = true })
  end

  return true
end

local function bundle_plugins(nvim_dir, pack_lock)
  io.write("bundling plugins\n")

  local pack_dir = path.join(nvim_dir, "share/nvim/site/pack/core/opt")
  local ok, err = execute("mkdir", {"mkdir", "-p", pack_dir})
  if not ok then
    return nil, err
  end

  local plugins = pack_lock.plugins
  for name, info in pairs(plugins) do
    local clone_ok, clone_err = clone_plugin(name, info, pack_dir)
    if not clone_ok then
      return nil, string.format("failed to clone %s: %s", name, clone_err)
    end
  end

  io.write("generating helptags\n")
  local nvim_bin = path.join(nvim_dir, "bin/nvim")
  ok, err = execute(nvim_bin, {nvim_bin, "--headless", "+helptags ALL", "+qa"}, { allow_failure = true })
  if not ok then
    io.write("  helptags skipped (cross-platform build)\n")
  end

  return true
end

local function verify_plugins(nvim_dir, pack_lock)
  io.write("verifying plugins\n")
  local nvim_bin = path.join(nvim_dir, "bin/nvim")

  local first_ok = execute(nvim_bin, {nvim_bin, "--version"}, { allow_failure = true })
  if not first_ok then
    io.write("  verification skipped (cross-platform build)\n")
    return true
  end

  for name, _ in pairs(pack_lock.plugins) do
    local cmd = string.format("+packadd %s", name)
    local ok, err = execute(nvim_bin, {nvim_bin, "--headless", cmd, "+qa"})
    if not ok then
      return nil, string.format("plugin %s failed to load: %s", name, err)
    end
  end

  return true
end

local function bundle(platform, nvim_dir)
  if not platform or platform == "" then
    return nil, "platform is required"
  end
  if not nvim_dir or nvim_dir == "" then
    return nil, "nvim_dir is required"
  end

  nvim_dir = nvim_dir:gsub("/$", "")

  local pack_lock, err = load_pack_lock(PACK_LOCK)
  if not pack_lock then
    return nil, err
  end

  local ok
  ok, err = bundle_plugins(nvim_dir, pack_lock)
  if not ok then
    return nil, err
  end

  ok, err = verify_plugins(nvim_dir, pack_lock)
  if not ok then
    return nil, err
  end

  io.write("bundle complete\n")
  return true
end

local M = {
  bundle = bundle,
  load_pack_lock = load_pack_lock,
}

if not pcall(debug.getlocal, 4, 1) then
  local platform = arg[1]
  local nvim_dir = arg[2]

  local ok, err = bundle(platform, nvim_dir)
  if not ok then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
  os.exit(0)
end

return M
