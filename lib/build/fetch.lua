-- lib/build/fetch.lua - unified version file loading, downloading, and extraction
local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("spawn").spawn

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

local function execute(args)
  local handle, err = spawn(args)
  if not handle then
    return nil, string.format("command failed to start: %s", err or "unknown error")
  end
  local exit_code, wait_err = handle:wait()
  if not exit_code then
    return nil, string.format("command failed: %s", wait_err or "abnormal termination")
  end
  if exit_code ~= 0 then
    return nil, string.format("command failed with exit %d", exit_code)
  end
  return true
end

-- extraction

local S_IXUSR = tonumber("100", 8) -- 0o100 = 64

local function move_executables_to_bin(output_dir)
  local bin_dir = path.join(output_dir, "bin")
  local bin_stat = unix.stat(bin_dir)
  if bin_stat and unix.S_ISDIR(bin_stat:mode()) then
    return true -- bin/ already exists
  end

  local executables = {}
  for name in unix.opendir(output_dir) do
    if name ~= "." and name ~= ".." then
      local entry_path = path.join(output_dir, name)
      local st = unix.stat(entry_path)
      if st and unix.S_ISREG(st:mode()) then
        local mode = st:mode()
        if mode & S_IXUSR ~= 0 then
          table.insert(executables, name)
        end
      end
    end
  end

  if #executables > 0 then
    unix.makedirs(bin_dir)
    for _, name in ipairs(executables) do
      local src = path.join(output_dir, name)
      local dest = path.join(bin_dir, name)
      unix.rename(src, dest)
      unix.chmod(dest, tonumber("755", 8))
    end
  end

  return true
end

local function extract_targz(archive_path, output_dir, strip_components)
  local ok, err = execute({
    "tar", "-xzf", archive_path, "-C", output_dir,
    "--strip-components=" .. (strip_components or 0)
  })
  if not ok then
    return nil, err
  end
  unix.unlink(archive_path)
  move_executables_to_bin(output_dir)
  return true
end

local function extract_zip(archive_path, output_dir, strip_components)
  local ok, err = execute({"unzip", "-q", "-DD", "-o", archive_path, "-d", output_dir})
  if not ok then
    return nil, err
  end

  if strip_components == 1 then
    local first_dir
    for name in unix.opendir(output_dir) do
      if name ~= "." and name ~= ".." then
        local entry_path = path.join(output_dir, name)
        local st = unix.stat(entry_path)
        if st and unix.S_ISDIR(st:mode()) then
          first_dir = entry_path
          break
        end
      end
    end
    if first_dir then
      ok, err = execute({"cp", "-r", first_dir .. "/.", output_dir})
      if not ok then
        return nil, err
      end
      unix.rmrf(first_dir)
    end
  end

  move_executables_to_bin(output_dir)
  unix.unlink(archive_path)
  return true
end

local function extract_gz(archive_path, binary_name, output_dir)
  local ok, err = execute({"gunzip", "-f", archive_path})
  if not ok then
    return nil, err
  end

  local bin_dir = path.join(output_dir, "bin")
  unix.makedirs(bin_dir)

  local uncompressed = path.join(output_dir, binary_name)
  local dest = path.join(bin_dir, binary_name)
  unix.rename(uncompressed, dest)
  unix.chmod(dest, tonumber("755", 8))
  return true
end

local function extract_binary(file_path, binary_name, output_dir)
  local bin_dir = path.join(output_dir, "bin")
  unix.makedirs(bin_dir)

  local dest = path.join(bin_dir, binary_name)
  unix.rename(file_path, dest)
  unix.chmod(dest, tonumber("755", 8))
  return true
end

local function extract(archive_path, output_dir, format, strip_components, binary_name)
  if format == "tar.gz" then
    return extract_targz(archive_path, output_dir, strip_components)
  elseif format == "zip" then
    return extract_zip(archive_path, output_dir, strip_components)
  elseif format == "gz" then
    return extract_gz(archive_path, binary_name, output_dir)
  elseif format == "binary" then
    return extract_binary(archive_path, binary_name, output_dir)
  else
    return nil, "unknown format: " .. tostring(format)
  end
end

-- build config from version data

local function build_config(version_data, key, platform)
  local vars = {
    version = version_data.version or "",
    tag = version_data.tag or "",
    date = version_data.date or "",
  }

  local sha, format, strip_components

  if version_data.binaries then
    -- simple format: binaries = {name = sha}
    sha = version_data.binaries[key]
    format = "binary"
    strip_components = 0
    vars.binary = key
  elseif version_data.platforms and platform then
    -- platform format: platforms = {platform = {sha, arch, os, ...}}
    local plat = version_data.platforms[platform]
    if not plat then
      return nil, string.format("platform %s not found", platform)
    end
    sha = plat.sha
    format = plat.format or version_data.format or "tar.gz"
    strip_components = plat.strip_components or version_data.strip_components or 0
    vars.platform = plat.platform or platform
    for k, v in pairs(plat) do
      if type(v) == "string" and k ~= "sha" and k ~= "format" then
        vars[k] = v
      end
    end
  else
    return nil, "version data must have 'binaries' or 'platforms'"
  end

  if not sha then
    return nil, "no sha found for " .. key
  end

  return {
    url = interpolate(version_data.url, vars),
    sha = sha,
    format = format,
    strip_components = strip_components,
  }
end

-- high-level fetch

local function fetch_binary(version_data, binary, dest_path)
  local config, err = build_config(version_data, binary, nil)
  if not config then
    return nil, err
  end

  local ok
  ok, err = download(config.url, dest_path)
  if not ok then
    return nil, err
  end

  ok, err = verify_sha256(dest_path, config.sha)
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

local function write_file(file_path, content)
  local fd = unix.open(file_path, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("644", 8))
  if not fd or fd < 0 then
    return nil, "failed to open " .. file_path
  end
  unix.write(fd, content)
  unix.close(fd)
  return true
end

local function fetch_tool(version_data, tool_name, platform, output_dir)
  local config, err = build_config(version_data, tool_name, platform)
  if not config then
    return nil, err
  end

  unix.makedirs(output_dir)

  local archive_name = config.format == "binary" and tool_name or ("archive." .. config.format)
  local archive_path = path.join(output_dir, archive_name)

  local ok
  ok, err = download(config.url, archive_path)
  if not ok then
    return nil, err
  end

  ok, err = verify_sha256(archive_path, config.sha)
  if not ok then
    unix.unlink(archive_path)
    return nil, err
  end

  ok, err = extract(archive_path, output_dir, config.format, config.strip_components, tool_name)
  if not ok then
    return nil, err
  end

  -- write manifest
  local manifest = string.format(
    'return {\n  version = %q,\n  sha = %q,\n}\n',
    version_data.version or "",
    config.sha
  )
  write_file(path.join(output_dir, ".extracted"), manifest)

  return true
end

-- CLI
if not pcall(debug.getlocal, 4, 1) then
  local version_path = arg[1]
  local key = arg[2]
  local arg3 = arg[3]
  local arg4 = arg[4]

  if not version_path or not key then
    io.stderr:write("usage: fetch.lua <version_file> <binary> <dest_path>\n")
    io.stderr:write("       fetch.lua <version_file> <tool> <platform> <output_dir>\n")
    os.exit(1)
  end

  local version_data, err = load_version(version_path)
  if not version_data then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end

  local ok
  if arg4 then
    ok, err = fetch_tool(version_data, key, arg3, arg4)
  else
    ok, err = fetch_binary(version_data, key, arg3)
  end

  if not ok then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
end

return {
  interpolate = interpolate,
  load_version = load_version,
  download = download,
  verify_sha256 = verify_sha256,
  make_executable = make_executable,
  extract = extract,
  build_config = build_config,
  fetch_binary = fetch_binary,
  fetch_tool = fetch_tool,
}
