local platform = require("platform")
local file = require("file")
local cosmo = require("cosmo")
local unix = cosmo.unix
local posix = require("posix")
local unistd = require("posix.unistd")
local stat = require("posix.sys.stat")
local dirent = require("posix.dirent")
local openssl = require("openssl")
local digest = require("openssl.digest")

local M = {}

local function stderr_write(msg)
  io.stderr:write(msg .. "\n")
  io.stderr:flush()
end

local function get_home_dir()
  return os.getenv("HOME") or os.getenv("USERPROFILE")
end

function M.get_shimlink_root()
  return file.path_join(get_home_dir(), ".local", "share", "shimlink")
end

function M.get_storage_dir(executable_name)
  if executable_name then
    return file.path_join(M.get_shimlink_root(), "_", executable_name)
  end
  return file.path_join(M.get_shimlink_root(), "_")
end

function M.get_versioned_dir(executable_name, sha256)
  return file.path_join(M.get_storage_dir(executable_name), sha256:sub(1, 16))
end

function M.get_shimlink_bin_dir()
  return file.path_join(M.get_shimlink_root(), "bin")
end

function M.get_shimlink_bin_link(executable_name)
  return file.path_join(M.get_shimlink_bin_dir(), executable_name)
end

function M.get_relative_versioned_path(executable_name, sha256, executable_path)
  local short_sha = sha256:sub(1, 16)
  if executable_path then
    return file.path_join("..", "_", executable_name, short_sha, executable_path)
  else
    return file.path_join("..", "_", executable_name, short_sha)
  end
end

function M.config_dir()
  return os.getenv("HOME") .. "/.config/shimlink"
end

function M.get_version_files()
  local entries, err = dirent.dir(M.config_dir())
  if not entries then
    return nil, "failed to read config directory: " .. tostring(err)
  end

  local files = {}
  for _, entry in ipairs(entries) do
    if entry:match("%.lua$") then
      table.insert(files, M.config_dir() .. "/" .. entry)
    end
  end

  return files
end

function M.load_all_configs()
  local files, err = M.get_version_files()
  if not files then
    error("failed to get version files: " .. tostring(err))
  end

  for _, path in ipairs(files) do
    local ok, load_err = platform.load_file(path)
    if not ok then
      error("failed to load " .. path .. ": " .. tostring(load_err))
    end
  end
end

function M.load_config(executable)
  local config = platform.get(executable)
  if not config then
    error("config not found for: " .. executable)
  end
  return config
end

function M.calculate_sha256(filepath)
  local f, err = io.open(filepath, "rb")
  if not f then
    return nil, "failed to open file for checksum: " .. tostring(err)
  end

  local md = digest.new("sha256")
  while true do
    local chunk = f:read(8192)
    if not chunk then break end
    md:update(chunk)
  end
  f:close()

  local hash = md:final()
  return hash:gsub(".", function(c) return string.format("%02x", string.byte(c)) end)
end

function M.download_file(url, dest_path)
  stderr_write("shimlink: downloading " .. url)

  local http_request = require("http.request")

  local req = http_request.new_from_uri(url)
  req.headers:upsert(":method", "GET")
  req.headers:upsert("user-agent", "shimlink/1.0")
  req.version = 1.1  -- Force HTTP/1.1 to avoid HTTP/2 bugs with large files
  req.follow_redirects = true  -- Ensure redirects are followed for GitHub releases
  req.max_redirects = 10

  local headers, stream = req:go()
  if not headers then
    return nil, "failed to connect: " .. tostring(stream)
  end

  local status = headers:get(":status")
  if status ~= "200" then
    return nil, "http error " .. status
  end

  local f, open_err = io.open(dest_path, "wb")
  if not f then
    return nil, "failed to open destination file: " .. tostring(open_err)
  end

  local ok, err = stream:save_body_to_file(f)
  f:close()

  if not ok then
    return nil, "failed to save response: " .. tostring(err)
  end

  return true
end

function M.extract_archive(archive_path, extract_to, executable_path)
  local archive_name = file.basename(archive_path)

  if archive_name:match("%.zip$") then
    local exit_status = posix.spawn({"unzip", "-q", archive_path, "-d", extract_to})
    if exit_status ~= 0 then
      return nil, "failed to extract zip"
    end
  elseif archive_name:match("%.tar%.gz$") or archive_name:match("%.tgz$") then
    local exit_status = posix.spawn({"tar", "-xzf", archive_path, "-C", extract_to})
    if exit_status ~= 0 then
      return nil, "failed to extract tar.gz"
    end
  elseif archive_name:match("%.gz$") and not archive_name:match("%.tar%.gz$") then
    if not executable_path then
      return nil, "executable_path required for single file gzip"
    end
    local output_path = file.path_join(extract_to, executable_path)
    file.mkdir_p(file.dirname(output_path))
    local exit_status = posix.spawn({"/bin/sh", "-c", string.format("gunzip -c %q > %q", archive_path, output_path)})
    if exit_status ~= 0 then
      return nil, "failed to extract gzip"
    end
    return output_path
  else
    return nil, "unsupported archive format: " .. archive_path
  end

  if executable_path then
    return file.path_join(extract_to, executable_path)
  else
    return archive_path
  end
end

function M.create_symlink_atomic(target, link_path)
  local temp_link = string.format("%s.tmp.%d.%d", link_path, os.time(), unistd.getpid())

  local cleanup = function()
    unix.unlink(temp_link)
  end

  local ok, err = pcall(function()
    file.mkdir_p(file.dirname(link_path))

    cleanup()

    local result = unix.symlink(target, temp_link)
    if not result then
      error(string.format("failed to create temporary symlink %s -> %s", temp_link, target))
    end

    local rename_result = os.rename(temp_link, link_path)
    if not rename_result then
      error(string.format("failed to rename symlink %s to %s", temp_link, link_path))
    end
  end)

  if not ok then
    cleanup()
    return nil, tostring(err)
  end

  return true
end

function M.create_symlink(bin_dir, src_path, dest_path, executable_name)
  local source = file.path_join(bin_dir, src_path)

  if not file.exists(source) then
    stderr_write("shimlink: warning: symlink source does not exist: " .. source)
    return
  end

  local dest = file.expand_path(dest_path)

  file.mkdir_p(file.dirname(dest))

  if file.exists(dest) then
    unix.unlink(dest)
  end

  local result = unix.symlink(source, dest)
  if result then
    stderr_write("shimlink: created symlink " .. dest .. " -> " .. source)
  else
    stderr_write("shimlink: warning: failed to create symlink " .. dest)
  end
end

function M.get_executables(config)
  if config.executables then
    return config.executables
  else
    return {{
      name = config.name,
      path = config.path,
      symlinks = config.symlinks,
      exec = config.exec,
    }}
  end
end

function M.update_config_checksum(executable_name, plat, new_sha)
  local config = platform.get(executable_name)
  if not config then
    return nil, "config not found for " .. executable_name
  end

  if not config.platforms[plat] then
    config.platforms[plat] = {}
  end
  config.platforms[plat].sha256 = new_sha

  local source = config._meta.source

  local reloaded = {}
  local kinds = {
    Version = function(cfg)
      if cfg.name == config.name then
        table.insert(reloaded, config)
      else
        table.insert(reloaded, cfg)
      end
    end
  }

  platform.load_file(source, kinds)

  local output = {}
  for _, cfg in ipairs(reloaded) do
    table.insert(output, platform.write(cfg))
  end

  local f, err = io.open(source, "w")
  if not f then
    return nil, "failed to open " .. source .. " for writing: " .. tostring(err)
  end
  f:write(table.concat(output, "\n"))
  f:close()

  stderr_write("shimlink: updated checksum for " .. executable_name .. " (" .. plat .. ") to " .. new_sha)
  return true
end

function M.download_to_temp(url, executable_name, temp_dir)
  local download_name = file.basename(url)
  local download_path = file.path_join(temp_dir, download_name)

  local ok, err = M.download_file(url, download_path)
  if not ok then
    return nil, "failed to download " .. executable_name .. ": " .. tostring(err)
  end

  return download_path
end

function M.validate_checksum(download_path, expected_sha256, executable_name, config, force)
  local actual_sha256, err = M.calculate_sha256(download_path)
  if not actual_sha256 then
    return nil, err
  end

  if force then
    local ok, update_err = M.update_config_checksum(executable_name, config.platform, actual_sha256)
    if not ok then
      return nil, update_err
    end
    stderr_write("shimlink: force mode - updated checksum for " .. executable_name)
  elseif expected_sha256 then
    if actual_sha256 ~= expected_sha256 then
      stderr_write("shimlink: checksum mismatch for " .. executable_name)
      stderr_write("shimlink:   expected: " .. expected_sha256)
      stderr_write("shimlink:   actual:   " .. actual_sha256)
      return nil, "checksum mismatch"
    end
  end

  return actual_sha256
end

function M.extract_and_prepare(download_path, temp_dir, path, executable_name, config)
  local extract_temp

  if path then
    extract_temp = file.path_join(temp_dir, "extracted")
    file.mkdir_p(extract_temp)
    local extracted_path, err = M.extract_archive(download_path, extract_temp, path)
    if not extracted_path then
      return nil, "failed to extract " .. executable_name .. ": " .. err
    end

    local strip_components = config.strip_components or 0
    local binary_dir = extract_temp
    for i = 1, strip_components do
      local first_entry, dir_err = file.list_dir_first_entry(binary_dir)
      if not first_entry then
        return nil, "failed to strip component " .. i .. ": " .. tostring(dir_err)
      end
      binary_dir = file.path_join(binary_dir, first_entry)
    end
    local executable_path = file.path_join(binary_dir, path)

    if file.exists(executable_path) then
      stat.chmod(executable_path, tonumber("0755", 8))
    end
  else
    extract_temp = temp_dir
    if file.exists(download_path) then
      stat.chmod(download_path, tonumber("0755", 8))
    end
  end

  return extract_temp
end

function M.install_to_versioned_dir(extract_temp, download_path, temp_dir, executable_name, path, config, actual_sha256)
  local versioned_dir = M.get_versioned_dir(executable_name, actual_sha256)
  local storage_dir = M.get_storage_dir(executable_name)
  file.mkdir_p(storage_dir)

  if file.is_directory(versioned_dir) then
    stderr_write("shimlink: version " .. actual_sha256:sub(1, 16) .. " already exists")
    return actual_sha256
  end

  local install_temp = file.path_join(temp_dir, "install")

  if path then
    local strip_components = config.strip_components or 0

    if strip_components > 0 then
      local source_dir = extract_temp
      for i = 1, strip_components do
        local first_entry, dir_err = file.list_dir_first_entry(source_dir)
        if not first_entry then
          return nil, "failed to strip component " .. i .. " during installation: " .. tostring(dir_err)
        end
        source_dir = file.path_join(source_dir, first_entry)
      end
      local ok, err = os.rename(source_dir, install_temp)
      if not ok then
        return nil, "failed to prepare installation directory: " .. tostring(err)
      end
    else
      local ok, err = os.rename(extract_temp, install_temp)
      if not ok then
        return nil, "failed to prepare installation directory: " .. tostring(err)
      end
    end
  else
    file.mkdir_p(install_temp)
    local final_path = file.path_join(install_temp, executable_name)
    local ok, err = os.rename(download_path, final_path)
    if not ok then
      return nil, "failed to move executable: " .. tostring(err)
    end
  end

  local temp_versioned = file.path_join(storage_dir, ".installing." .. actual_sha256:sub(1, 16))
  local ok, err = os.rename(install_temp, temp_versioned)
  if not ok then
    return nil, "failed to create temporary versioned directory: " .. tostring(err)
  end

  ok, err = os.rename(temp_versioned, versioned_dir)
  if not ok then
    local cleanup_ok, cleanup_err = file.rm_rf(temp_versioned)
    if not cleanup_ok then
      stderr_write("shimlink: warning: failed to cleanup temp directory: " .. tostring(cleanup_err))
    end
    return nil, "failed to finalize installation: " .. tostring(err)
  end

  stderr_write("shimlink: installed " .. executable_name .. " version " .. actual_sha256:sub(1, 16))

  return actual_sha256
end

function M.download_executable(executable_name, config, force, register_temp_dir_callback)
  local url = config.url
  local expected_sha256 = config.sha256
  local path = config.path

  local shimlink_root = M.get_shimlink_root()
  file.mkdir_p(shimlink_root)

  local stdlib = require("posix.stdlib")
  local temp_template = file.path_join(shimlink_root, ".tmp." .. executable_name .. ".XXXXXX")
  local temp_dir = stdlib.mkdtemp(temp_template)
  if register_temp_dir_callback then
    register_temp_dir_callback(temp_dir)
  end

  local success, result = pcall(function()
    local download_path, err = M.download_to_temp(url, executable_name, temp_dir)
    if not download_path then
      error(err)
    end

    local actual_sha256
    actual_sha256, err = M.validate_checksum(download_path, expected_sha256, executable_name, config, force)
    if not actual_sha256 then
      error(err)
    end

    local extract_temp
    extract_temp, err = M.extract_and_prepare(download_path, temp_dir, path, executable_name, config)
    if not extract_temp then
      error(err)
    end

    local installed_sha
    installed_sha, err = M.install_to_versioned_dir(extract_temp, download_path, temp_dir, executable_name, path, config, actual_sha256)
    if not installed_sha then
      error(err)
    end

    return installed_sha
  end)

  local cleanup_ok, cleanup_err = file.rm_rf(temp_dir)
  if not cleanup_ok then
    stderr_write("shimlink: warning: failed to cleanup temp directory: " .. tostring(cleanup_err))
  end

  if not success then
    stderr_write("shimlink: " .. tostring(result))
    return false
  end

  return true, result
end

function M.update_executable(executable_name, config, force, register_temp_dir_callback)
  local plat = platform.detect()
  local platform_config, err = platform.get_platform_config(config, plat)
  if not platform_config then
    stderr_write("shimlink: " .. err)
    return false
  end

  local executables = M.get_executables(platform_config)

  local sha256
  local expected_sha256 = platform_config.sha256
  if expected_sha256 and not force then
    local versioned_dir = M.get_versioned_dir(executable_name, expected_sha256)
    if file.is_directory(versioned_dir) then
      stderr_write("shimlink: version " .. expected_sha256:sub(1, 16) .. " already exists")
      sha256 = expected_sha256
    end
  end

  if not sha256 then
    local success
    local urls = config.urls or {}
    platform_config.url = urls[plat] or config.url
    success, sha256 = M.download_executable(executable_name, platform_config, force, register_temp_dir_callback)
    if not success then
      return false
    end
  end

  local versioned_dir = M.get_versioned_dir(executable_name, sha256)

  for _, executable_config in ipairs(executables) do
    local executable_name_inner = executable_config.name
    local executable_path_config = executable_config.path
    local executable_symlinks = executable_config.symlinks

    local executable_full_path
    if executable_path_config then
      executable_full_path = file.path_join(versioned_dir, executable_path_config)
    else
      executable_full_path = file.path_join(versioned_dir, executable_name_inner)
    end

    if not file.exists(executable_full_path) then
      stderr_write("shimlink: executable not found at " .. executable_full_path)
      return false
    end

    local shimlink_bin_link = M.get_shimlink_bin_link(executable_name_inner)

    local executable_path = executable_path_config or executable_name_inner
    local relative_to_versioned = M.get_relative_versioned_path(executable_name, sha256, executable_path)

    local ok, err = M.create_symlink_atomic(relative_to_versioned, shimlink_bin_link)
    if not ok then
      stderr_write("shimlink: failed to create shimlink bin link for " .. executable_name_inner .. ": " .. tostring(err))
      return false
    end
    stderr_write("shimlink: updated " .. shimlink_bin_link .. " -> " .. relative_to_versioned)

    if executable_symlinks then
      for src_path, dest_path in pairs(executable_symlinks) do
        M.create_symlink(versioned_dir, src_path, dest_path, executable_name_inner)
      end
    end
  end

  return true
end

return M
