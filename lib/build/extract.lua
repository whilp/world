#!/usr/bin/env lua
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("spawn").spawn

local function move_contents(source_dir, dest_dir)
  local dir = unix.opendir(source_dir)
  if not dir then
    return nil, "failed to open directory " .. source_dir
  end
  for name in dir do
    if name ~= "." and name ~= ".." then
      local src = path.join(source_dir, name)
      local dst = path.join(dest_dir, name)
      local ok, err = unix.rename(src, dst)
      if not ok then
        return nil, "failed to move " .. src .. " to " .. dst .. ": " .. tostring(err)
      end
    end
  end
  return true
end

local function strip_components(source_dir, dest_dir, strip)
  if strip == 0 then
    return move_contents(source_dir, dest_dir)
  end

  -- navigate down (strip - 1) levels, then move contents of the final level
  local current = source_dir
  for i = 1, strip - 1 do
    local dir = unix.opendir(current)
    if not dir then
      return nil, "failed to open directory " .. current
    end
    local entries = {}
    for name in dir do
      if name ~= "." and name ~= ".." then
        table.insert(entries, name)
      end
    end
    if #entries ~= 1 then
      local msg = "cannot strip %d components: expected 1 entry at level %d, found %d"
      return nil, string.format(msg, strip, i, #entries)
    end
    local entry = path.join(current, entries[1])
    local stat = unix.stat(entry)
    if not stat then
      return nil, string.format("cannot strip %d components: failed to stat entry at level %d", strip, i)
    end
    if not unix.S_ISDIR(stat:mode()) then
      return nil, string.format("cannot strip %d components: entry at level %d is not a directory", strip, i)
    end
    current = entry
  end

  -- at final level, find the single entry and move it (file or directory)
  local dir = unix.opendir(current)
  if not dir then
    return nil, "failed to open directory " .. current
  end
  local entries = {}
  for name in dir do
    if name ~= "." and name ~= ".." then
      table.insert(entries, name)
    end
  end
  if #entries ~= 1 then
    local msg = "cannot strip %d components: expected 1 entry at level %d, found %d"
    return nil, string.format(msg, strip, strip, #entries)
  end
  local final_entry = path.join(current, entries[1])
  local stat = unix.stat(final_entry)
  if not stat then
    return nil, string.format("cannot strip %d components: failed to stat entry at level %d", strip, strip)
  end

  if unix.S_ISDIR(stat:mode()) then
    return move_contents(final_entry, dest_dir)
  else
    local dst = path.join(dest_dir, entries[1])
    local ok, err = unix.rename(final_entry, dst)
    if not ok then
      return nil, "failed to move " .. final_entry .. " to " .. dst .. ": " .. tostring(err)
    end
    return true
  end
end

local function clear_dir(dir)
  local handle = unix.opendir(dir)
  if not handle then return end
  for name in handle do
    if name ~= "." and name ~= ".." then
      unix.rmrf(path.join(dir, name))
    end
  end
end

local function extract_zip(archive, dest_dir, strip)
  strip = strip or 0
  local temp_dir = dest_dir .. ".tmp"
  unix.rmrf(temp_dir)
  unix.makedirs(temp_dir)
  clear_dir(dest_dir)

  -- use absolute path to avoid cosmopolitan binary issues with unveil
  local handle = spawn({"/usr/bin/unzip", "-o", "-d", temp_dir, archive})
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    unix.rmrf(temp_dir)
    return nil, "unzip failed with exit code " .. exit_code
  end

  local ok, err = strip_components(temp_dir, dest_dir, strip)
  unix.rmrf(temp_dir)
  if not ok then
    return nil, err
  end
  return true
end

local function extract_targz(archive, dest_dir, strip)
  strip = strip or 0
  local temp_dir = dest_dir .. ".tmp"
  unix.rmrf(temp_dir)
  unix.makedirs(temp_dir)
  clear_dir(dest_dir)

  local handle = spawn({"tar", "-xzf", archive, "-C", temp_dir})
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    unix.rmrf(temp_dir)
    return nil, "tar failed with exit code " .. exit_code
  end

  local ok, err = strip_components(temp_dir, dest_dir, strip)
  unix.rmrf(temp_dir)
  if not ok then
    return nil, err
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
  -- TODO: re-enable unveil once we handle temp_dir inode changes after rmrf
  -- unveil binds to inode, so rmrf+makedirs breaks access

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
