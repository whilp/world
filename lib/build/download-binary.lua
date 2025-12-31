#!/usr/bin/env lua
-- Downloads a binary from version metadata
-- Usage: lua lib/build/download-binary.lua <version_file> <binary_name> <output_path>

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local function interpolate(template, vars)
  if type(template) ~= "string" then
    return template
  end
  return template:gsub("{([%w_]+)}", function(key)
    return tostring(vars[key] or "")
  end)
end

local function download_binary(version_file, binary_name, output_path)
  if not version_file or version_file == "" then
    return nil, "version_file is required"
  end
  if not binary_name or binary_name == "" then
    return nil, "binary_name is required"
  end
  if not output_path or output_path == "" then
    return nil, "output_path is required"
  end

  local ok, version_info = pcall(dofile, version_file)
  if not ok then
    return nil, "failed to load " .. version_file .. ": " .. tostring(version_info)
  end

  local expected_sha = version_info.binaries and version_info.binaries[binary_name]
  if not expected_sha then
    return nil, string.format("no checksum for binary '%s' in %s", binary_name, version_file)
  end

  local url = interpolate(version_info.url, {
    version = version_info.version,
    binary = binary_name,
  })

  local status, headers, body
  local last_err
  local max_attempts = 8
  local fetch_opts = {headers = {["User-Agent"] = "curl/8.0"}, maxresponse = 300 * 1024 * 1024}
  for attempt = 1, max_attempts do
    status, headers, body = cosmo.Fetch(url, fetch_opts)
    if status then
      break
    end
    last_err = tostring(headers or "unknown error")
    if attempt < max_attempts then
      local delay = math.min(30, 2 ^ attempt)
      unix.nanosleep(delay, 0)
    end
  end
  if not status then
    return nil, "fetch failed: " .. last_err
  end
  if status ~= 200 then
    return nil, "fetch failed with status " .. tostring(status)
  end

  local actual_sha = cosmo.EncodeHex(cosmo.Sha256(body)):lower()
  if actual_sha ~= expected_sha:lower() then
    return nil, string.format("sha256 mismatch: expected %s, got %s", expected_sha, actual_sha)
  end

  unix.makedirs(path.dirname(output_path))

  local fd = unix.open(output_path, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("755", 8))
  if not fd or fd < 0 then
    return nil, "failed to open " .. output_path
  end
  local bytes_written = unix.write(fd, body)
  unix.close(fd)
  if bytes_written ~= #body then
    return nil, "failed to write data"
  end

  return true
end

local M = {
  download_binary = download_binary,
  interpolate = interpolate,
}

if not pcall(debug.getlocal, 4, 1) then
  local version_file = arg[1]
  local binary_name = arg[2]
  local output_path = arg[3]

  local ok, err = download_binary(version_file, binary_name, output_path)
  if not ok then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
  os.exit(0)
end

return M
