#!/usr/bin/env lua
local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local function copy_file(src, dest)
  local content = cosmo.Slurp(src)
  if not content then
    return nil, "failed to read " .. src
  end
  unix.makedirs(path.dirname(dest))
  local fd = unix.open(dest, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("755", 8))
  if not fd or fd < 0 then
    return nil, "failed to open " .. dest
  end
  unix.write(fd, content)
  unix.close(fd)
  return true
end

local function main(version_file, platform, src, base_dir)
  if not version_file or not platform or not src or not base_dir then
    return nil, "usage: install.lua <version_file> <platform> <src> <base_dir>"
  end

  local ok, spec = pcall(dofile, version_file)
  if not ok then
    return nil, "failed to load " .. version_file .. ": " .. tostring(spec)
  end

  local plat = spec.platforms[platform]
  if not plat then
    return nil, "unknown platform: " .. platform
  end

  local tool = path.basename(base_dir)
  local sha8 = plat.sha:sub(1, 8)
  local version_dir = spec.version .. "-" .. sha8
  local dest = path.join(base_dir, version_dir, "bin", tool)

  local err
  ok, err = copy_file(src, dest)
  if not ok then
    return nil, err
  end

  local link_path = path.join(base_dir, "bin")
  unix.unlink(link_path)
  unix.symlink(path.join(version_dir, "bin"), link_path)

  return true
end

if not pcall(debug.getlocal, 4, 1) then
  local ok, err = main(...)
  if not ok then
    io.stderr:write("error: " .. err .. "\n")
    os.exit(1)
  end
end
