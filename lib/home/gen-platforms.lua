local cosmo = require("cosmo")
local unix = cosmo.unix
local path = cosmo.path

local home = require("main")

local function file_size(filepath)
  local st = unix.stat(filepath)
  if not st then
    return nil, "failed to stat: " .. filepath
  end
  return st:size()
end

local function sha256_file(filepath)
  local shasum = unix.commandv("shasum") or unix.commandv("sha256sum")
  if not shasum then
    return nil, "neither shasum nor sha256sum found"
  end

  local cmd
  if shasum:match("shasum$") then
    cmd = string.format("%s -a 256 %q 2>/dev/null", shasum, filepath)
  else
    cmd = string.format("%s %q 2>/dev/null", shasum, filepath)
  end

  local f = io.popen(cmd)
  if not f then
    return nil, "failed to run shasum"
  end

  local output = f:read("*a")
  f:close()

  local sha = output:match("^(%x+)")
  if not sha or #sha ~= 64 then
    return nil, "invalid sha256 output"
  end
  return sha
end

local function extract_manifest(asset_path)
  local unzip = unix.commandv("unzip")
  if not unzip then
    return nil, "unzip not found"
  end

  local cmd = string.format("%s -p %q manifest.lua 2>/dev/null", unzip, asset_path)
  local f = io.popen(cmd)
  if not f then
    return nil, "failed to run unzip"
  end

  local output = f:read("*a")
  f:close()

  if not output or output == "" then
    return nil, "no manifest.lua found in " .. asset_path
  end

  local fn, err = load(output)
  if not fn then
    return nil, "failed to parse manifest: " .. (err or "unknown error")
  end

  local load_ok, manifest = pcall(fn)
  if not load_ok then
    return nil, "failed to execute manifest: " .. tostring(manifest)
  end

  return manifest
end

local function platform_from_asset(asset_path)
  local basename = path.basename(asset_path)
  local platform = basename:match("^home%-(.+)$")
  return platform
end

local function cmd_help()
  io.stderr:write("usage: gen-platforms <base_url> <tag> <asset1> [asset2] ...\n")
  io.stderr:write("\n")
  io.stderr:write("generates platforms.lua from platform asset files\n")
  io.stderr:write("extracts sha256, size, and manifest from each asset\n")
  io.stderr:write("output is written to stdout\n")
  return 0
end

local function main(args)
  if #args < 3 or args[1] == "help" or args[1] == "--help" then
    return cmd_help()
  end

  local base_url = args[1]
  local tag = args[2]
  local platforms_data = {}

  for i = 3, #args do
    local asset_path = args[i]
    local platform = platform_from_asset(asset_path)
    if not platform then
      io.stderr:write("warning: cannot determine platform from: " .. asset_path .. "\n")
    else
      local sha256, err = sha256_file(asset_path)
      if not sha256 then
        io.stderr:write("error: " .. err .. "\n")
        return 1
      end

      local size
      size, err = file_size(asset_path)
      if not size then
        io.stderr:write("error: " .. err .. "\n")
        return 1
      end

      local manifest
      manifest, err = extract_manifest(asset_path)
      if not manifest then
        io.stderr:write("error: " .. err .. "\n")
        return 1
      end

      platforms_data[platform] = {
        asset = "home-" .. platform,
        sha256 = sha256,
        size = size,
        manifest = manifest,
      }
    end
  end

  local output = {
    base_url = base_url,
    tag = tag,
    platforms = platforms_data,
  }
  io.write(home.serialize_table(output))
  return 0
end

local M = {
  file_size = file_size,
  sha256_file = sha256_file,
  extract_manifest = extract_manifest,
  platform_from_asset = platform_from_asset,
  main = main,
}

if not pcall(debug.getlocal, 4, 1) then
  os.exit(main(arg) or 0)
end

return M
