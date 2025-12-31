-- lib/build/fetch.lua - generic version file loading, interpolation, and downloading
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

local function load_version(version_path)
  if not version_path or version_path == "" then
    return nil, "version_path cannot be empty"
  end
  local ok, data = pcall(dofile, version_path)
  if not ok then
    return nil, "failed to load " .. version_path .. ": " .. tostring(data)
  end
  return data
end

local function build_url(version_data, binary)
  if not version_data then
    return nil, "version_data is required"
  end
  if not binary or binary == "" then
    return nil, "binary name is required"
  end
  if not version_data.url then
    return nil, "version_data.url is required"
  end
  local vars = {
    version = version_data.version or "",
    binary = binary,
  }
  return interpolate(version_data.url, vars)
end

local function get_sha(version_data, binary)
  if not version_data or not version_data.binaries then
    return nil, "version_data.binaries is required"
  end
  return version_data.binaries[binary]
end

local function download(url, dest_path)
  if not url or url == "" then
    return nil, "url cannot be empty"
  end
  if not dest_path or dest_path == "" then
    return nil, "dest_path cannot be empty"
  end

  local parent = path.dirname(dest_path)
  if parent and parent ~= "" and parent ~= "." then
    unix.makedirs(parent)
  end

  local status, headers, body
  local last_err
  local max_attempts = 8
  local fetch_opts = {
    headers = {["User-Agent"] = "curl/8.0"},
    maxresponse = 300 * 1024 * 1024,
  }

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

  local fd = unix.open(dest_path, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("644", 8))
  if not fd or fd < 0 then
    return nil, "failed to open destination file"
  end
  local bytes_written = unix.write(fd, body)
  unix.close(fd)
  if bytes_written ~= #body then
    return nil, "failed to write data"
  end

  return true
end

local function verify_sha256(file_path, expected_sha)
  if not file_path or file_path == "" then
    return nil, "file_path cannot be empty"
  end
  if not expected_sha or expected_sha == "" then
    return nil, "expected_sha cannot be empty"
  end

  local content = cosmo.Slurp(file_path)
  if not content then
    return nil, "failed to read file"
  end

  local actual = cosmo.EncodeHex(cosmo.Sha256(content)):lower()
  if actual == expected_sha:lower() then
    return true
  end
  return nil, string.format("sha256 mismatch: expected %s, got %s", expected_sha, actual)
end

local function make_executable(file_path)
  if not file_path or file_path == "" then
    return nil, "file_path cannot be empty"
  end
  unix.chmod(file_path, tonumber("755", 8))
  return true
end

local function fetch_binary(version_data, binary, dest_path)
  local url, err = build_url(version_data, binary)
  if not url then
    return nil, err
  end

  local sha = get_sha(version_data, binary)
  if not sha then
    return nil, "no sha256 for binary: " .. binary
  end

  local ok
  ok, err = download(url, dest_path)
  if not ok then
    return nil, err
  end

  ok, err = verify_sha256(dest_path, sha)
  if not ok then
    unix.unlink(dest_path)
    return nil, err
  end

  ok, err = make_executable(dest_path)
  if not ok then
    return nil, err
  end

  return true
end

-- CLI
if not pcall(debug.getlocal, 4, 1) then
  local version_path = arg[1]
  local binary = arg[2]
  local dest_path = arg[3]

  if not version_path or not binary or not dest_path then
    io.stderr:write("usage: fetch.lua <version_file> <binary> <dest_path>\n")
    os.exit(1)
  end

  local version_data, err = load_version(version_path)
  if not version_data then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end

  local ok
  ok, err = fetch_binary(version_data, binary, dest_path)
  if not ok then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
end

return {
  interpolate = interpolate,
  load_version = load_version,
  build_url = build_url,
  get_sha = get_sha,
  download = download,
  verify_sha256 = verify_sha256,
  make_executable = make_executable,
  fetch_binary = fetch_binary,
}
