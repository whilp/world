local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("spawn").spawn

local function interpolate(template, vars)
  return template:gsub("{([%w_]+)}", function(key)
    return tostring(vars[key] or "")
  end)
end

local function fetch(url)
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

local function write_file(dest, content, mode)
  unix.makedirs(path.dirname(dest))
  local fd = unix.open(dest, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, mode)
  if not fd or fd < 0 then return nil, "failed to open " .. dest end
  local written = unix.write(fd, content)
  unix.close(fd)
  if written ~= #content then return nil, "write failed" end
  return true
end

local function main(version_file, platform, output)
  if not version_file or not platform or not output then
    return nil, "usage: fetch2.lua <version_file> <platform> <output>"
  end

  local ok, spec = pcall(dofile, version_file)
  if not ok then
    return nil, "failed to load " .. version_file .. ": " .. tostring(spec)
  end

  local plat = spec.platforms[platform]
  if not plat then
    return nil, "unknown platform: " .. platform
  end

  local vars = {version = spec.version, arch = plat.arch}
  local url = interpolate(spec.url, vars)

  local body, err = fetch(url)
  if not body then
    return nil, err
  end

  ok, err = verify_sha256(body, plat.sha)
  if not ok then
    return nil, err
  end

  local format = plat.format or spec.format or "binary"
  local base_dir = output
  local tool = path.basename(base_dir)
  local sha8 = plat.sha:sub(1, 8)
  local version_dir = spec.version .. "-" .. sha8
  local dest_dir = path.join(base_dir, version_dir)

  if format == "binary" then
    local bin_path = path.join(dest_dir, "bin", tool)
    ok, err = write_file(bin_path, body, tonumber("755", 8))
    if not ok then
      return nil, err
    end
  elseif format == "zip" then
    local archive_path = path.join(base_dir, "archive.zip")
    ok, err = write_file(archive_path, body, tonumber("644", 8))
    if not ok then
      return nil, err
    end

    local bin_dir = path.join(dest_dir, "bin")
    unix.makedirs(bin_dir)

    local handle = spawn({"unzip", "-o", "-d", bin_dir, archive_path})
    local exit_code = handle:wait()
    if exit_code ~= 0 then
      return nil, "unzip failed with exit code " .. exit_code
    end

    unix.unlink(archive_path)
  else
    return nil, "unknown format: " .. format
  end

  local link_path = path.join(base_dir, "bin")
  unix.unlink(link_path)
  unix.symlink(path.join(version_dir, "bin"), link_path)

  return true
end

if not pcall(debug.getlocal, 4, 1) then
  local ok, err = main(...)
  if not ok then
    io.stderr:write("error: " .. err .. "\n")
    os.exit(1)
  end
end
