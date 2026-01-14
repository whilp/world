#!/usr/bin/env lua
-- build-zip.tl: create versioned .zip artifacts for 3p modules

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")

local record PlatformSpec
  sha: string
  format: string
end

local record VersionSpec
  version: string
  platforms: {string:PlatformSpec}
end

local function main(version_file: string, platform: string, staged_symlink: string, output_zip: string): boolean, string
  if not version_file or not platform or not staged_symlink or not output_zip then
    return nil, "usage: build-zip.lua <version_file> <platform> <staged_symlink> <output.zip>"
  end

  local load_ok, spec = pcall(dofile, version_file) as (boolean, VersionSpec)
  if not load_ok then
    return nil, "failed to load " .. version_file .. ": " .. tostring(spec)
  end

  local plat = spec.platforms[platform] or spec.platforms["*"]
  if not plat then
    return nil, "unknown platform: " .. platform
  end

  -- derive module name from staged_symlink path: o/<module>/.staged
  local staged_dir = path.dirname(staged_symlink)
  local module_name = path.basename(staged_dir)

  -- make staged_symlink absolute for unix.readlink
  local staged_symlink_abs: string
  if staged_symlink:sub(1, 1) == "/" then
    staged_symlink_abs = staged_symlink
  else
    local cwd = unix.getcwd()
    staged_symlink_abs = path.join(cwd, staged_symlink)
  end

  -- read the staged directory path
  local staged_path = unix.readlink(staged_symlink_abs, 1024)
  if not staged_path then
    return nil, "failed to read symlink " .. staged_symlink
  end

  -- resolve to absolute path
  local staged_abs: string
  if staged_path:sub(1, 1) == "/" then
    staged_abs = staged_path
  else
    staged_abs = path.join(path.dirname(staged_symlink), staged_path)
  end

  -- get versioned name from staged path
  local versioned_name = path.basename(staged_abs)

  -- create a temporary directory for building the zip structure
  local temp_dir = unix.mkdtemp("/tmp/build-zip_XXXXXX")
  if not temp_dir then
    return nil, "failed to create temp directory"
  end

  -- create the target structure: <module>/<versioned_name>/
  local zip_root = path.join(temp_dir, module_name, versioned_name)
  local ok_mk = unix.makedirs(zip_root)
  if not ok_mk then
    unix.rmrf(temp_dir)
    return nil, "failed to create directory structure"
  end

  -- copy contents from staged directory to zip root
  -- use cp -a to preserve symlinks and permissions
  local handle = spawn({"cp", "-a", staged_abs .. "/.", zip_root})
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    unix.rmrf(temp_dir)
    return nil, "failed to copy staged files"
  end

  -- create the zip file
  -- use -r for recursive, -y for symlinks
  unix.makedirs(path.dirname(output_zip))
  unix.rmrf(output_zip)

  -- cd to temp_dir and zip from there to get relative paths
  local cwd = unix.getcwd()
  unix.chdir(temp_dir)
  local zip_handle = spawn({"zip", "-qry", path.join(cwd, output_zip), module_name})
  local zip_exit = zip_handle:wait()
  unix.chdir(cwd)
  unix.rmrf(temp_dir)

  if zip_exit ~= 0 then
    return nil, "zip command failed with exit code " .. zip_exit
  end

  -- format: ZIPPED  module @ version-sha_prefix
  local sha_prefix = plat.sha:sub(1, 7)
  io.stderr:write(string.format("□ ZIPPED %s @ %s-%s\n", module_name, spec.version, sha_prefix))

  return true
end

if cosmo.is_main() then
  local ok, err = main(arg[1], arg[2], arg[3], arg[4])
  if not ok then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
end
