#!/usr/bin/env lua
-- ast-grep ignore: builds relative symlink paths
-- teal ignore: type annotations needed
-- stage.lua: extract fetched archives based on version.lua format

local cosmo = require("cosmo")
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

local function copy_binary(archive, dest_dir, module_name)
  local bin_dir = path.join(dest_dir, "bin")
  unix.makedirs(bin_dir)
  local dest = path.join(bin_dir, module_name)
  local content = io.open(archive, "rb"):read("*a")
  local fd = unix.open(dest, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("755", 8))
  if not fd then
    return nil, "failed to create " .. dest
  end
  unix.write(fd, content)
  unix.close(fd)
  return true
end

local function extract_gz(archive, dest_dir, module_name)
  local bin_dir = path.join(dest_dir, "bin")
  unix.makedirs(bin_dir)
  local dest = path.join(bin_dir, module_name)
  local handle = spawn({"gunzip", "-c", archive})
  local exit_code, content = handle:read()
  if not exit_code then
    return nil, "gunzip failed: " .. tostring(content)
  end
  local fd = unix.open(dest, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("755", 8))
  if not fd then
    return nil, "failed to create " .. dest
  end
  unix.write(fd, content)
  unix.close(fd)
  return true
end

local function install_files(source_dir, dest_dir, strip_prefix)
  local src = source_dir
  if strip_prefix then
    src = path.join(source_dir, strip_prefix)
  end
  return move_contents(src, dest_dir)
end

local function find_archive(fetch_dir, format)
  if format == "binary" or format == "gz" then
    return path.join(fetch_dir, "binary")
  end
  local ext = format == "zip" and ".zip" or format == "tar.gz" and ".tar.gz" or nil
  local dir = unix.opendir(fetch_dir)
  if not dir then
    return nil, "failed to open " .. fetch_dir
  end
  for name in dir do
    if name ~= "." and name ~= ".." then
      if not ext or name:sub(-#ext) == ext then
        return path.join(fetch_dir, name)
      end
    end
  end
  return nil, "no archive found in " .. fetch_dir
end

local function main(version_file, platform, input, output)
  if not version_file or not platform or not input or not output then
    return nil, "usage: stage.lua <version_file> <platform> <input.fetched> <output.staged>"
  end

  local stage_o = os.getenv("STAGE_O")
  if not stage_o then
    return nil, "STAGE_O env var required"
  end

  local output_dir = path.dirname(output)
  unix.makedirs(output_dir)
  unix.makedirs(stage_o)
  unix.unveil(version_file, "r")
  unix.unveil(input, "r")
  unix.unveil(output_dir, "rwc")
  unix.unveil(stage_o, "rwc")
  unix.unveil("/usr/bin", "rx")
  unix.unveil(nil, nil)

  local ok, spec = pcall(dofile, version_file)
  if not ok then
    return nil, "failed to load " .. version_file .. ": " .. tostring(spec)
  end

  local plat = spec.platforms[platform] or spec.platforms["*"]
  if not plat then
    return nil, "unknown platform: " .. platform
  end

  local format = spec.format or plat.format or "binary"
  local strip = spec.strip_components or 1

  -- derive module name from output path: o/<module>/.staged
  local output_dir = path.dirname(output)
  local module_name = path.basename(output_dir)

  -- input is now a directory; find the archive inside
  local archive, err = find_archive(input, format)
  if not archive then
    return nil, err
  end

  local version_sha = spec.version .. "-" .. plat.sha

  local stage_dir = path.join(stage_o, module_name, version_sha)
  local ok_mk, err_mk = unix.makedirs(stage_dir)
  if not ok_mk then
    return nil, "makedirs failed for " .. stage_dir .. ": " .. tostring(err_mk)
  end

  if format == "zip" then
    local temp_dir = unix.mkdtemp(path.join(path.dirname(stage_dir), ".stage_XXXXXX"))
    ok, err = extract_zip(archive, temp_dir, strip)
    if ok then
      ok, err = install_files(temp_dir, stage_dir, spec.strip_prefix)
    end
    unix.rmrf(temp_dir)
  elseif format == "tar.gz" then
    local temp_dir = unix.mkdtemp(path.join(path.dirname(stage_dir), ".stage_XXXXXX"))
    ok, err = extract_targz(archive, temp_dir, strip)
    if ok then
      ok, err = install_files(temp_dir, stage_dir, spec.strip_prefix)
    end
    unix.rmrf(temp_dir)
  elseif format == "binary" then
    ok, err = copy_binary(archive, stage_dir, module_name)
  elseif format == "gz" then
    ok, err = extract_gz(archive, stage_dir, module_name)
  else
    return nil, "unknown format: " .. format
  end

  if not ok then
    return nil, err
  end

  -- format: STAGE  module @ version-sha_prefix
  local sha_prefix = plat.sha:sub(1, 7)
  io.stderr:write(string.format("□ STAGE  %s @ %s-%s\n", module_name, spec.version, sha_prefix))

  -- create symlink: output -> staged/<module>/<version-sha>
  -- output is o/<module>/.staged, staged is o/staged/<module>/<ver>-<sha>
  unix.rmrf(output)
  unix.makedirs(output_dir)
  local stage_o_basename = stage_o:match("([^/]+)$")
  local rel_path = "../" .. stage_o_basename .. "/" .. module_name .. "/" .. version_sha
  local link_ok, link_err = unix.symlink(rel_path, output)
  if not link_ok then
    return nil, "failed to symlink: " .. tostring(link_err)
  end

  return true
end

if cosmo.is_main() then
  local ok, err = main(...)
  if not ok then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
end
