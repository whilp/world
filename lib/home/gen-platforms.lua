local cosmo = require("cosmo")
local unix = cosmo.unix
local path = cosmo.path

local function sha256_file(filepath)
  local shasum = unix.commandv("shasum") or unix.commandv("sha256sum")
  if not shasum then
    return nil, "neither shasum nor sha256sum found"
  end

  local cmd
  if shasum:match("shasum$") then
    cmd = string.format("%s -a 256 %q", shasum, filepath)
  else
    cmd = string.format("%s %q", shasum, filepath)
  end

  local f = io.popen(cmd)
  if not f then
    return nil, "failed to run " .. shasum
  end

  local output = f:read("*a")
  f:close()

  local sha = output:match("^(%x+)")
  if not sha or #sha ~= 64 then
    return nil, "failed to compute sha256 for " .. filepath
  end
  return sha
end

local function file_size(filepath)
  local st = unix.stat(filepath)
  if not st then
    return nil, "failed to stat: " .. filepath
  end
  return st:size()
end

local function extract_manifest(asset_path)
  local unzip = unix.commandv("unzip")
  if not unzip then
    return nil, "unzip not found"
  end

  local cmd = string.format("%s -p %q manifest.lua", unzip, asset_path)
  local f = io.popen(cmd)
  if not f then
    return nil, "failed to run unzip"
  end

  local manifest_str = f:read("*a")
  f:close()

  if not manifest_str or manifest_str == "" then
    return nil, "no manifest.lua found in " .. asset_path
  end

  local fn, err = load(manifest_str)
  if not fn then
    return nil, "failed to parse manifest: " .. (err or "unknown error")
  end

  local ok, manifest = pcall(fn)
  if not ok then
    return nil, "failed to execute manifest: " .. tostring(manifest)
  end

  return manifest
end

local function platform_from_asset(asset_path)
  local basename = path.basename(asset_path)
  local platform = basename:match("^home%-(.+)$")
  return platform
end

local function format_manifest_table(manifest, indent)
  indent = indent or "      "
  local lines = {}
  table.insert(lines, "{")

  if manifest.version then
    table.insert(lines, indent .. string.format('  version = "%s",', manifest.version))
  end

  if manifest.files then
    table.insert(lines, indent .. "  files = {")
    local paths = {}
    for p in pairs(manifest.files) do
      table.insert(paths, p)
    end
    table.sort(paths)
    for _, p in ipairs(paths) do
      local info = manifest.files[p]
      table.insert(lines, indent .. string.format('    ["%s"] = { mode = %d },', p, info.mode))
    end
    table.insert(lines, indent .. "  },")
  end

  table.insert(lines, indent .. "}")
  return table.concat(lines, "\n")
end

local function format_platforms(platforms_data, base_url, tag)
  local lines = {}
  table.insert(lines, "return {")
  table.insert(lines, string.format('  base_url = "%s",', base_url))
  table.insert(lines, string.format('  tag = "%s",', tag))
  table.insert(lines, "  platforms = {")

  local platform_names = {}
  for name in pairs(platforms_data) do
    table.insert(platform_names, name)
  end
  table.sort(platform_names)

  for _, name in ipairs(platform_names) do
    local info = platforms_data[name]
    table.insert(lines, string.format('    ["%s"] = {', name))
    table.insert(lines, string.format('      asset = "%s",', info.asset))
    table.insert(lines, string.format('      sha256 = "%s",', info.sha256))
    table.insert(lines, string.format('      size = %d,', info.size))
    table.insert(lines, "      manifest = " .. format_manifest_table(info.manifest, "      ") .. ",")
    table.insert(lines, "    },")
  end

  table.insert(lines, "  },")
  table.insert(lines, "}")
  return table.concat(lines, "\n")
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

  local output = format_platforms(platforms_data, base_url, tag)
  io.write(output .. "\n")
  return 0
end

local M = {
  sha256_file = sha256_file,
  extract_manifest = extract_manifest,
  platform_from_asset = platform_from_asset,
  format_platforms = format_platforms,
  main = main,
}

if not pcall(debug.getlocal, 4, 1) then
  os.exit(main(arg) or 0)
end

return M
