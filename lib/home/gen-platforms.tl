local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local home = require("main")

local record Stat
  mode: function(self): number
  size: function(self): number
  mtim: function(self): number
end

local record SpawnHandle
  read: function(self: SpawnHandle, size?: number): boolean | string, string, number
end

local record ManifestData
  -- manifest content from inside the zip
end

local record PlatformInfo
  asset: string
  sha256: string
  size: number
end

local record PlatformsData
  base_url: string
  tag: string
  platforms: {string:PlatformInfo}
end

local function file_size(filepath: string): number, string
  local st = unix.stat(filepath) as Stat
  if not st then
    return nil, "failed to stat: " .. filepath
  end
  return st:size()
end

local function extract_manifest(asset_path: string): ManifestData, string
  local unzip = unix.commandv("unzip")
  if not unzip then
    return nil, "unzip not found"
  end

  local handle = home.spawn({ "unzip", "-p", asset_path, "manifest.lua" }) as SpawnHandle
  local ok, output = handle:read()
  if not ok or not output or output == "" then
    return nil, "no manifest.lua found in " .. asset_path
  end

  local fn, err = load(output as string)
  if not fn then
    return nil, "failed to parse manifest: " .. (err or "unknown error")
  end

  local load_ok, manifest = pcall(fn)
  if not load_ok then
    return nil, "failed to execute manifest: " .. tostring(manifest)
  end

  return manifest as ManifestData
end

local function platform_from_asset(asset_path: string): string
  local basename = path.basename(asset_path)
  local platform = basename:match("^home%-(.+)$")
  return platform
end

local function cmd_help(): number
  io.stderr:write("usage: gen-platforms <output_dir> <base_url> <tag> <asset1> [asset2] ...\n")
  io.stderr:write("\n")
  io.stderr:write("generates platforms.lua and per-platform manifest files\n")
  io.stderr:write("  <output_dir>/platforms.lua - platform metadata\n")
  io.stderr:write("  <output_dir>/manifests/<platform>.lua - per-platform manifests\n")
  return 0
end

local function main(args: {string}): number
  if #args < 4 or args[1] == "help" or args[1] == "--help" then
    return cmd_help()
  end

  local output_dir = args[1]
  local base_url = args[2]
  local tag = args[3]
  local platforms_data: {string:PlatformInfo} = {}

  unix.makedirs(path.join(output_dir, "manifests"))

  for i = 4, #args do
    local asset_path = args[i]
    local platform = platform_from_asset(asset_path)
    if not platform then
      io.stderr:write("warning: cannot determine platform from: " .. asset_path .. "\n")
    else
      local sha256, err = home.sha256_file(asset_path)
      if not sha256 then
        io.stderr:write("error: " .. err .. "\n")
        return 1
      end

      local size: number
      size, err = file_size(asset_path)
      if not size then
        io.stderr:write("error: " .. err .. "\n")
        return 1
      end

      local manifest: ManifestData
      manifest, err = extract_manifest(asset_path)
      if not manifest then
        io.stderr:write("error: " .. err .. "\n")
        return 1
      end

      local manifest_path = path.join(output_dir, "manifests", platform .. ".lua")
      local f = io.open(manifest_path, "w")
      if not f then
        io.stderr:write("error: failed to write " .. manifest_path .. "\n")
        return 1
      end
      f:write(home.serialize_table(manifest))
      f:close()

      platforms_data[platform] = {
        asset = "home-" .. platform,
        sha256 = sha256,
        size = size,
      }
    end
  end

  local platforms_path = path.join(output_dir, "platforms.lua")
  local f = io.open(platforms_path, "w")
  if not f then
    io.stderr:write("error: failed to write " .. platforms_path .. "\n")
    return 1
  end
  f:write(home.serialize_table({
    base_url = base_url,
    tag = tag,
    platforms = platforms_data,
  } as any))
  f:close()

  return 0
end

local record M
  file_size: function(filepath: string): number, string
  extract_manifest: function(asset_path: string): ManifestData, string
  platform_from_asset: function(asset_path: string): string
  main: function(args: {string}): number
end

local M_impl: M = {
  file_size = file_size,
  extract_manifest = extract_manifest,
  platform_from_asset = platform_from_asset,
  main = main,
}

if cosmo.is_main() then
  os.exit(main(arg) or 0)
end

return M_impl
