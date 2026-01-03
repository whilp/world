#!/usr/bin/env lua
-- stage.lua: extract fetched archives based on version.lua format

local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")

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

  local current = source_dir
  for i = 1, strip do
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
      return nil, string.format("strip %d: expected 1 entry at level %d, found %d", strip, i, #entries)
    end
    current = path.join(current, entries[1])
  end

  local stat = unix.stat(current)
  if stat and unix.S_ISDIR(stat:mode()) then
    return move_contents(current, dest_dir)
  else
    local dst = path.join(dest_dir, path.basename(current))
    return unix.rename(current, dst)
  end
end

local function extract_zip(archive, dest_dir, strip)
  local temp_dir = unix.mkdtemp(path.join(path.dirname(dest_dir), ".stage_XXXXXX"))
  local handle = spawn({"/usr/bin/unzip", "-o", "-q", "-d", temp_dir, archive})
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    unix.rmrf(temp_dir)
    return nil, "unzip failed with exit code " .. exit_code
  end
  local ok, err = strip_components(temp_dir, dest_dir, strip)
  unix.rmrf(temp_dir)
  return ok, err
end

local function extract_targz(archive, dest_dir, strip)
  local temp_dir = unix.mkdtemp(path.join(path.dirname(dest_dir), ".stage_XXXXXX"))
  local handle = spawn({"tar", "-xzf", archive, "-C", temp_dir})
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    unix.rmrf(temp_dir)
    return nil, "tar failed with exit code " .. exit_code
  end
  local ok, err = strip_components(temp_dir, dest_dir, strip)
  unix.rmrf(temp_dir)
  return ok, err
end

local function copy_binary(archive, dest_dir)
  local dest = path.join(dest_dir, path.basename(archive):gsub("%.fetched$", ""))
  local content = io.open(archive, "rb"):read("*a")
  local fd = unix.open(dest, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("755", 8))
  if not fd then
    return nil, "failed to create " .. dest
  end
  unix.write(fd, content)
  unix.close(fd)
  return true
end

local function install_files(source_dir, dest_dir, install)
  if not install then
    return move_contents(source_dir, dest_dir)
  end
  if type(install) == "string" then
    install = {install}
  end
  for _, file in ipairs(install) do
    local src = path.join(source_dir, file)
    local dst = path.join(dest_dir, file)
    unix.makedirs(path.dirname(dst))
    local ok, err = unix.rename(src, dst)
    if not ok then
      -- try copy if rename fails (cross-device)
      local content = io.open(src, "rb")
      if not content then
        return nil, "failed to read " .. src
      end
      local data = content:read("*a")
      content:close()
      local fd = unix.open(dst, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("644", 8))
      if not fd then
        return nil, "failed to create " .. dst
      end
      unix.write(fd, data)
      unix.close(fd)
    end
  end
  return true
end

local function main(version_file, input, output)
  if not version_file or not input or not output then
    return nil, "usage: stage.lua <version_file> <input.fetched> <output.staged>"
  end

  local ok, spec = pcall(dofile, version_file)
  if not ok then
    return nil, "failed to load " .. version_file .. ": " .. tostring(spec)
  end

  local format = spec.format or "binary"
  local strip = spec.strip_components or 1

  unix.makedirs(output)

  local err
  if format == "zip" then
    local temp_dir = unix.mkdtemp(path.join(path.dirname(output), ".stage_XXXXXX"))
    ok, err = extract_zip(input, temp_dir, strip)
    if ok then
      ok, err = install_files(temp_dir, output, spec.install)
    end
    unix.rmrf(temp_dir)
  elseif format == "tar.gz" then
    local temp_dir = unix.mkdtemp(path.join(path.dirname(output), ".stage_XXXXXX"))
    ok, err = extract_targz(input, temp_dir, strip)
    if ok then
      ok, err = install_files(temp_dir, output, spec.install)
    end
    unix.rmrf(temp_dir)
  elseif format == "binary" then
    ok, err = copy_binary(input, output)
  else
    return nil, "unknown format: " .. format
  end

  return ok, err
end

if not pcall(debug.getlocal, 4, 1) then
  local ok, err = main(...)
  if not ok then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
end
