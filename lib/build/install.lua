#!/usr/bin/env lua
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("spawn").spawn

local M = {}

-- copy a single file to target directory
-- if install_type is "bin" and source filename looks generic, rename to tool_name
function M.copy_file(source, target_dir, install_type, tool_name)
  local source_name = path.basename(source)
  local dest

  if install_type == "bin" then
    -- only rename if source name is generic (like "download", "binary", etc)
    local generic_names = { download = true, binary = true, bin = true }
    if generic_names[source_name] then
      dest = path.join(target_dir, tool_name)
    else
      dest = path.join(target_dir, source_name)
    end
  else
    dest = path.join(target_dir, source_name)
  end

  local exit_code = spawn({ "cp", source, dest }):wait()
  if exit_code ~= 0 then
    return nil, "cp failed with exit code " .. exit_code
  end
  return true
end

-- copy a directory's contents to target directory
function M.copy_dir(source, target_dir)
  local exit_code = spawn({ "cp", "-r", source, target_dir }):wait()
  if exit_code ~= 0 then
    return nil, "cp failed with exit code " .. exit_code
  end
  return true
end

-- main install function
function M.install(version_file, platform, base_dir, install_type, source)
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
  local tool_name = path.basename(base_dir)

  unix.makedirs(target_dir)

  local stat = unix.stat(source)
  if not stat then
    return nil, "source not found: " .. source
  end

  local err
  if unix.S_ISDIR(stat:mode()) then
    ok, err = M.copy_dir(source, target_dir)
  else
    ok, err = M.copy_file(source, target_dir, install_type, tool_name)
  end

  if not ok then
    return nil, err
  end

  -- create symlink
  local link_path = path.join(base_dir, install_type)
  unix.unlink(link_path)
  unix.symlink(path.join(version_dir, install_type), link_path)

  return true
end

local function main(version_file, platform, base_dir, install_type, source)
  if not version_file or not platform or not base_dir or not install_type or not source then
    return nil, "usage: install.lua <version_file> <platform> <base_dir> <bin|lib> <source>"
  end

  unix.unveil(version_file, "r")
  unix.unveil(source, "r")
  unix.unveil(base_dir, "rwc")
  unix.unveil("/usr", "rx")
  unix.unveil(nil, nil)

  return M.install(version_file, platform, base_dir, install_type, source)
end

if not pcall(debug.getlocal, 4, 1) then
  local ok, err = main(...)
  if not ok then
    io.stderr:write("error: " .. err .. "\n")
    os.exit(1)
  end
end

return M
