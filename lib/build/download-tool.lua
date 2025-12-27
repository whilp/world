#!/usr/bin/env lua
-- Downloads and extracts a tool for a specific platform
-- Usage: lua lib/build/download-tool.lua <tool> <platform> <output_dir>

local cosmo = require("cosmo")
local path = cosmo.path
local unix = cosmo.unix
local spawn = require("spawn").spawn

-- Template interpolation
local function interpolate(template, vars)
  if type(template) ~= "string" then
    return template
  end
  return template:gsub("{([%w_]+)}", function(key)
    return tostring(vars[key] or "")
  end)
end

-- Execute external command
local function execute(program, args)
  local handle, err = spawn(args)
  if not handle then
    return nil, string.format("command failed to start: %s (%s)", program, err or "unknown error")
  end
  local exit_code, wait_err = handle:wait()
  if not exit_code then
    return nil, string.format("command failed: %s (%s)", program, wait_err or "abnormal termination")
  end
  if exit_code ~= 0 then
    return nil, string.format("command failed: %s (exit: %d)", program, exit_code)
  end
  return true
end

-- Load tool metadata
local function load_tool_config(tool_name, platform)
  if not tool_name or tool_name == "" then
    return nil, "tool_name cannot be empty"
  end
  if not platform or platform == "" then
    return nil, "platform cannot be empty"
  end

  local version_file = path.join("3p", tool_name, "version.lua")
  local ok, tool = pcall(dofile, version_file)
  if not ok then
    return nil, "failed to load " .. version_file .. ": " .. tostring(tool)
  end

  local config = tool.platforms[platform]
  if not config then
    return nil, string.format("platform %s not found for tool %s", platform, tool_name)
  end

  -- Build interpolation context - merge tool-level and platform-level variables
  local vars = {
    version = tool.version or "",
    release_sha = tool.release_sha or "",
    date = tool.date or "",
    tag = tool.tag or "",
    platform = config.platform or platform,  -- allow platform override
  }

  -- Add platform-specific custom variables (arch, os, etc.)
  for key, value in pairs(config) do
    if type(value) == "string" and key ~= "sha" and key ~= "format" then
      vars[key] = value
    end
  end

  -- URL must be at tool level (no per-platform URLs)
  local url = tool.url
  if not url then
    return nil, "no URL specified for " .. tool_name
  end
  url = interpolate(url, vars)

  return {
    tool_name = tool_name,
    platform = platform,
    url = url,
    format = config.format or tool.format or "tar.gz",
    strip_components = config.strip_components or tool.strip_components or 0,
    sha = config.sha,
    version = tool.version,
  }
end

local function download_file(url, dest_path)
  if not url or url == "" then
    return nil, "url cannot be empty"
  end
  if not dest_path or dest_path == "" then
    return nil, "dest_path cannot be empty"
  end

  local status, _, body = cosmo.Fetch(url)
  if not status then
    return nil, "fetch failed: " .. tostring(body or "unknown error")
  end
  if status ~= 200 then
    return nil, "fetch failed with status " .. tostring(status)
  end

  local fd = unix.open(dest_path, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, 0644)
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

-- Verify SHA256 checksum
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

-- Extract tar.gz archive
local function extract_targz(archive_path, output_dir, strip_components)
  local ok, err = execute("/usr/bin/tar", {
    "tar", "-xzf", archive_path, "-C", output_dir,
    "--strip-components=" .. strip_components
  })
  if not ok then
    return nil, err
  end
  unix.unlink(archive_path)
  return true
end

-- Extract zip archive
local function extract_zip(archive_path, output_dir, strip_components)
  local ok, err = execute("/usr/bin/unzip", {"unzip", "-q", "-DD", "-o", archive_path, "-d", output_dir})
  if not ok then
    return nil, err
  end

  if strip_components == 1 then
    -- Move contents up one level using shell
    local sh_script = string.format([[
      cd '%s' && \
      dir=$(find . -mindepth 1 -maxdepth 1 -type d | head -1) && \
      if [ -n "$dir" ]; then \
        cp -r "$dir"/. . && \
        rm -rf "$dir"; \
      fi
    ]], output_dir)
    ok, err = execute("/bin/sh", {"sh", "-c", sh_script})
    if not ok then
      return nil, err
    end
  end

  unix.unlink(archive_path)
  return true
end

-- Extract gz compressed file
local function extract_gz(archive_path, tool_name, output_dir)
  local ok, err = execute("/usr/bin/gunzip", {"gunzip", "-f", archive_path})
  if not ok then
    return nil, err
  end

  local bin_dir = path.join(output_dir, "bin")
  unix.makedirs(bin_dir)

  local uncompressed_path = path.join(output_dir, tool_name)
  local binary_path = path.join(bin_dir, tool_name)
  unix.rename(uncompressed_path, binary_path)
  unix.chmod(binary_path, 493)
  return true
end

-- Make binary executable and move to bin subdirectory
local function make_executable(file_path, tool_name, output_dir)
  if not file_path or file_path == "" then
    return nil, "file_path cannot be empty"
  end

  local bin_dir = path.join(output_dir, "bin")
  unix.makedirs(bin_dir)

  local binary_path = path.join(bin_dir, tool_name)
  unix.rename(file_path, binary_path)
  unix.chmod(binary_path, 493)
  return true
end

-- Extract dispatch table
local extractors = {
  ["tar.gz"] = extract_targz,
  ["zip"] = extract_zip,
  ["gz"] = extract_gz,
  ["binary"] = make_executable,
}

-- Extract based on format
local function extract(archive_path, output_dir, config)
  local extractor = extractors[config.format]
  if not extractor then
    return nil, "unknown format: " .. config.format
  end

  if config.format == "gz" then
    return extractor(archive_path, config.tool_name, output_dir)
  elseif config.format == "binary" then
    return extractor(archive_path, config.tool_name, output_dir)
  else
    return extractor(archive_path, output_dir, config.strip_components)
  end
end

-- Write file with error checking
local function write_file(filepath, content)
  local fd = unix.open(filepath, unix.O_CREAT | unix.O_WRONLY | unix.O_TRUNC, 420)
  if not fd then
    return nil, "failed to open " .. filepath .. " for writing"
  end
  unix.write(fd, content)
  unix.close(fd)
  return true
end

-- Write metadata files
local function write_metadata(output_dir, config)
  local ok, err = write_file(path.join(output_dir, "VERSION"), config.version .. "\n")
  if not ok then
    return nil, err
  end

  local sha_short = config.sha:sub(1, 8)
  ok, err = write_file(path.join(output_dir, "SHA"), sha_short .. "\n")
  if not ok then
    return nil, err
  end

  return true
end

-- Main download and extract logic
local function download_tool(tool_name, platform, output_dir)
  -- Validate inputs
  if not tool_name or tool_name == "" then
    return nil, "tool_name is required"
  end
  if not platform or platform == "" then
    return nil, "platform is required"
  end
  if not output_dir or output_dir == "" then
    return nil, "output_dir is required"
  end

  -- Remove trailing slash
  output_dir = output_dir:gsub("/$", "")

  -- Load configuration
  local config, err = load_tool_config(tool_name, platform)
  if not config then
    return nil, err
  end

  -- Determine archive name and path
  local archive_name = config.format == "binary" and tool_name or ("archive." .. config.format)
  local archive_path = path.join(output_dir, archive_name)
  local cleanup_needed = false

  -- Transaction pattern: download and extract with cleanup on failure
  local ok, result = pcall(function()
    -- Download
    local download_ok, download_err = download_file(config.url, archive_path)
    if not download_ok then
      error(download_err)
    end
    cleanup_needed = true

    -- Verify checksum
    local verify_ok, verify_err = verify_sha256(archive_path, config.sha)
    if not verify_ok then
      error(verify_err)
    end

    -- Extract
    local extract_ok, extract_err = extract(archive_path, output_dir, config)
    if not extract_ok then
      error(extract_err)
    end
    cleanup_needed = false

    -- Write metadata
    local meta_ok, meta_err = write_metadata(output_dir, config)
    if not meta_ok then
      error(meta_err)
    end

    return true
  end)

  -- Cleanup on failure
  if not ok and cleanup_needed then
    pcall(unix.unlink, archive_path)
  end

  if not ok then
    return nil, tostring(result)
  end
  return true
end

-- Module exports
local M = {
  download_tool = download_tool,
  interpolate = interpolate,
  load_tool_config = load_tool_config,
}

-- Main entry point when executed directly
if not pcall(debug.getlocal, 4, 1) then
  local tool_name = arg[1]
  local platform = arg[2]
  local output_dir = arg[3]

  local ok, err = download_tool(tool_name, platform, output_dir)
  if not ok then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
  os.exit(0)
end

return M
