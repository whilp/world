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

local function list_dir(dir)
  local files = {}
  local d = unix.opendir(dir)
  if not d then return files end
  while true do
    local name = d:read()
    if not name then break end
    if name ~= "." and name ~= ".." then
      table.insert(files, name)
    end
  end
  d:close()
  return files
end

local function main(version_file, platform, src, base_dir)
  if not version_file or not platform or not src or not base_dir then
    return nil, "usage: install.lua <version_file> <platform> <src> <base_dir>"
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
  local bin_dir = path.join(base_dir, version_dir, "bin")

  local stat = unix.stat(src)
  local is_dir = stat and unix.S_ISDIR(stat:mode())

  local binaries
  if is_dir then
    binaries = list_dir(src)
  else
    binaries = {path.basename(base_dir)}
  end

  for _, tool in ipairs(binaries) do
    local src_path = is_dir and path.join(src, tool) or src
    local dest = path.join(bin_dir, tool)
    local err
    ok, err = copy_file(src_path, dest)
    if not ok then
      return nil, err
    end
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
