#!/usr/bin/env lua
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("spawn").spawn

local function extract_zip(archive, dest_dir, strip)
  -- use absolute path to avoid cosmopolitan binary issues with unveil
  local handle = spawn({"/usr/bin/unzip", "-o", "-d", dest_dir, archive})
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return nil, "unzip failed with exit code " .. exit_code
  end

  -- TODO: unzip doesn't support strip-components, so we do it manually
  -- Consider using a zip library or more robust path manipulation
  if strip and strip > 0 then
    -- find the single top-level directory and move its contents up
    local dir = unix.opendir(dest_dir)
    if not dir then
      return nil, "failed to open " .. dest_dir
    end
    local entries = {}
    for name in dir do
      if name ~= "." and name ~= ".." then
        table.insert(entries, name)
      end
    end
    if #entries == 1 then
      local subdir = path.join(dest_dir, entries[1])
      local subdir_handle = unix.opendir(subdir)
      if subdir_handle then
        -- it's a directory, move contents up
        for name in subdir_handle do
          if name ~= "." and name ~= ".." then
            local src = path.join(subdir, name)
            local dst = path.join(dest_dir, name)
            unix.rename(src, dst)
          end
        end
        unix.rmdir(subdir)
      end
    end
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
  local handle = spawn({"gunzip", "-c", archive})
  local ok, output, exit_code = handle:read()
  if not ok then
    return nil, "gunzip failed with exit code " .. (exit_code or "unknown")
  end
  local fd = unix.open(dest, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("755", 8))
  if not fd then
    return nil, "failed to create " .. dest
  end
  unix.write(fd, output)
  unix.close(fd)
  return true
end

local function main(version_file, platform, input, dest_dir)
  if not version_file or not platform or not input or not dest_dir then
    return nil, "usage: extract.lua <version_file> <platform> <input> <dest_dir>"
  end

  unix.makedirs(dest_dir)
  unix.unveil(version_file, "r")
  unix.unveil(input, "r")
  unix.unveil(dest_dir, "rwc")
  unix.unveil("/usr", "rx")
  unix.unveil("/bin", "rx")
  unix.unveil(nil, nil)

  local ok, spec = pcall(dofile, version_file)
  if not ok then
    return nil, "failed to load " .. version_file .. ": " .. tostring(spec)
  end

  local plat = spec.platforms[platform] or spec.platforms["*"]
  if not plat then
    return nil, "unknown platform: " .. platform
  end

  local format = plat.format or spec.format or "binary"

  local strip = plat.strip_components or spec.strip_components or 0

  local err
  if format == "zip" then
    ok, err = extract_zip(input, dest_dir, strip)
  elseif format == "tar.gz" then
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
