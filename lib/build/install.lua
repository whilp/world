#!/usr/bin/env lua
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("spawn").spawn

local function main(version_file, platform, base_dir, install_type, source)
  if not version_file or not platform or not base_dir or not install_type or not source then
    return nil, "usage: install.lua <version_file> <platform> <base_dir> <bin|lib> <source>"
  end

  local ok, spec = pcall(dofile, version_file)
  if not ok then
    return nil, "failed to load " .. version_file .. ": " .. tostring(spec)
  end

  local plat = spec.platforms[platform] or spec.platforms["*"]
  if not plat then
    return nil, "unknown platform: " .. platform
  end

  local version_dir = spec.version .. "-" .. plat.sha:sub(1, 8)
  local target_dir = path.join(base_dir, version_dir, install_type)

  unix.makedirs(target_dir)

  -- copy source to target_dir
  local stat = unix.stat(source)
  if not stat then
    return nil, "source not found: " .. source
  end

  local is_dir = unix.S_ISDIR(stat:mode())
  local cp_args = is_dir and { "cp", "-r", source, target_dir } or { "cp", source, target_dir }
  local exit_code = spawn(cp_args):wait()
  if exit_code ~= 0 then
    return nil, "cp failed with exit code " .. exit_code
  end

  -- create symlink
  local link_path = path.join(base_dir, install_type)
  unix.unlink(link_path)
  unix.symlink(path.join(version_dir, install_type), link_path)

  return true
end

if not pcall(debug.getlocal, 4, 1) then
  local ok, err = main(...)
  if not ok then
    io.stderr:write("error: " .. err .. "\n")
    os.exit(1)
  end
end
