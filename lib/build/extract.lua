#!/usr/bin/env lua
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("spawn").spawn

local function extract_zip(archive, dest_dir)
  local handle = spawn({"unzip", "-o", "-d", dest_dir, archive})
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return nil, "unzip failed with exit code " .. exit_code
  end
  return true
end

local function extract_targz(archive, dest_dir, strip)
  local args = {"tar", "-xzf", archive, "-C", dest_dir}
  if strip and strip > 0 then
    table.insert(args, "--strip-components=" .. strip)
  end
  local handle = spawn(args)
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return nil, "tar failed with exit code " .. exit_code
  end
  return true
end

local function extract_gz(archive, dest_dir, tool_name)
  local dest = path.join(dest_dir, tool_name)
  local handle = spawn({"sh", "-c", "gunzip -c " .. archive .. " > " .. dest})
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return nil, "gunzip failed with exit code " .. exit_code
  end
  unix.chmod(dest, tonumber("755", 8))
  return true
end

local function main(version_file, platform, input, dest_dir)
  if not version_file or not platform or not input or not dest_dir then
    return nil, "usage: extract.lua <version_file> <platform> <input> <dest_dir>"
  end

  local ok, spec = pcall(dofile, version_file)
  if not ok then
    return nil, "failed to load " .. version_file .. ": " .. tostring(spec)
  end

  local plat = spec.platforms[platform] or spec.platforms["*"]
  if not plat then
    return nil, "unknown platform: " .. platform
  end

  local format = plat.format or spec.format or "binary"

  unix.makedirs(dest_dir)

  local err
  if format == "zip" then
    ok, err = extract_zip(input, dest_dir)
  elseif format == "tar.gz" then
    local strip = plat.strip_components or spec.strip_components or 0
    ok, err = extract_targz(input, dest_dir, strip)
  elseif format == "gz" then
    local tool_name = path.basename(path.dirname(dest_dir))
    ok, err = extract_gz(input, dest_dir, tool_name)
  else
    return nil, "unknown format: " .. format
  end

  if not ok then
    return nil, err
  end

  return true
end

if not pcall(debug.getlocal, 4, 1) then
  local ok, err = main(...)
  if not ok then
    io.stderr:write("error: " .. err .. "\n")
    os.exit(1)
  end
end
