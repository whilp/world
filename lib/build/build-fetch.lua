#!/usr/bin/env lua
local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local function interpolate(template, vars)
  return template:gsub("{([%w_]+)}", function(key)
    return tostring(vars[key] or "")
  end)
end

local function build_url(spec, platform)
  local plat = spec.platforms[platform] or spec.platforms["*"]
  if not plat then
    return nil, "unknown platform: " .. platform
  end

  local vars = {platform = platform}
  for k, v in pairs(spec) do
    if type(v) ~= "table" then
      vars[k] = v
    end
  end
  for k, v in pairs(plat) do
    vars[k] = v
  end

  return interpolate(spec.url, vars)
end

local function download(url)
  local status, headers, body
  local last_err
  local opts = {headers = {["User-Agent"] = "curl/8.0"}, maxresponse = 100 * 1024 * 1024}

  for attempt = 1, 8 do
    status, headers, body = cosmo.Fetch(url, opts)
    if status then break end
    last_err = tostring(headers or "unknown")
    if attempt < 8 then unix.nanosleep(math.min(30, 2 ^ attempt), 0) end
  end

  if not status then return nil, "fetch failed: " .. last_err end
  if status ~= 200 then return nil, "fetch status " .. status end
  return body
end

local function verify_sha256(content, expected)
  local actual = cosmo.EncodeHex(cosmo.Sha256(content)):lower()
  if actual == expected:lower() then return true end
  return nil, string.format("sha256 mismatch: expected %s, got %s", expected, actual)
end

local function main(version_file, platform, output)
  if not version_file or not platform or not output then
    return nil, "usage: fetch.lua <version_file> <platform> <output>"
  end

  local output_dir = path.dirname(output)
  local fetch_o = os.getenv("FETCH_O")
  if not fetch_o then
    return nil, "FETCH_O env var required"
  end
  unix.makedirs(output_dir)
  unix.makedirs(fetch_o)
  unix.unveil(version_file, "r")
  unix.unveil(output_dir, "rwc")
  unix.unveil(fetch_o, "rwc")
  unix.unveil("/etc/resolv.conf", "r")
  unix.unveil("/etc/ssl", "r")
  unix.unveil(nil, nil)

  local ok, spec = pcall(dofile, version_file)
  if not ok then
    return nil, "failed to load " .. version_file .. ": " .. tostring(spec)
  end

  local plat = spec.platforms[platform] or spec.platforms["*"]
  if not plat then
    return nil, "unknown platform: " .. platform
  end

  local url = build_url(spec, platform)

  local body, err = download(url)
  if not body then
    return nil, err
  end

  ok, err = verify_sha256(body, plat.sha)
  if not ok then
    return nil, err
  end

  -- build archive path: $FETCH_O/<version>-<sha>/<basename from url>
  local archive_name = url:match("([^/]+)$")
  local archive_dir = path.join(fetch_o, spec.version .. "-" .. plat.sha)
  local archive_path = path.join(archive_dir, archive_name)

  io.stderr:write(archive_path .. ": " .. url .. "\n")

  unix.makedirs(archive_dir)

  if not cosmo.Barf(archive_path, body, tonumber("644", 8)) then
    return nil, "failed to write " .. archive_path
  end

  -- remove old symlink/file if exists, create relative symlink
  -- output is o/<prefix>/version.lua.fetched, archive is o/archive/...
  -- count path components to determine depth
  unix.unlink(output)
  local depth = select(2, output_dir:gsub("/", ""))
  local up = string.rep("../", depth)
  local fetch_o_basename = fetch_o:match("([^/]+)$")
  local rel_path = up .. fetch_o_basename .. "/" .. spec.version .. "-" .. plat.sha .. "/" .. archive_name
  local link_ok, link_err = unix.symlink(rel_path, output)
  if not link_ok then
    return nil, "failed to symlink: " .. tostring(link_err)
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

return {
  build_url = build_url,
}
