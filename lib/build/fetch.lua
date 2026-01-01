#!/usr/bin/env lua
local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local function interpolate(template, vars)
  return template:gsub("{([%w_]+)}", function(key)
    return tostring(vars[key] or "")
  end)
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

local function main(version_file, platform, output, binary)
  if not version_file or not platform or not output then
    return nil, "usage: fetch.lua <version_file> <platform> <output> [binary]"
  end

  local output_dir = path.dirname(output)
  unix.makedirs(output_dir)
  unix.unveil(version_file, "r")
  unix.unveil(output_dir, "rwc")
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

  local vars = {binary = binary}
  -- copy top-level spec fields (version, tag, etc)
  for k, v in pairs(spec) do
    if type(v) ~= "table" then
      vars[k] = v
    end
  end
  -- copy platform-specific fields (override top-level)
  for k, v in pairs(plat) do
    vars[k] = v
  end
  local url = interpolate(spec.url, vars)

  local body, err = download(url)
  if not body then
    return nil, err
  end

  ok, err = verify_sha256(body, plat.sha)
  if not ok then
    return nil, err
  end

  if not cosmo.Barf(output, body, tonumber("755", 8)) then
    return nil, "failed to write " .. output
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
