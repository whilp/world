#!/usr/bin/env lua
-- Downloads and extracts a tool for a specific platform
-- Usage: lua lib/build/download-tool.lua <tool> <platform> <output_dir>

-- Template interpolation
local function interpolate(template, vars)
  if type(template) ~= "string" then
    return template
  end
  return template:gsub("%${([%w_]+)}", function(key)
    return tostring(vars[key] or "")
  end)
end

-- Load tool metadata
local function load_tool_config(tool_name, platform)
  if not tool_name or tool_name == "" then
    return nil, "tool_name cannot be empty"
  end
  if not platform or platform == "" then
    return nil, "platform cannot be empty"
  end

  local version_file = string.format("3p/%s/version.lua", tool_name)
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

-- Execute command with error checking
local function execute(cmd)
  local success, exit_type, code = os.execute(cmd)
  if not success then
    return nil, string.format("command failed: %s (exit: %s %s)", cmd, exit_type, code)
  end
  return true
end

-- Download file from URL
local function download_file(url, dest_path)
  if not url or url == "" then
    return nil, "url cannot be empty"
  end
  if not dest_path or dest_path == "" then
    return nil, "dest_path cannot be empty"
  end

  local cmd = string.format("curl -fsSL -o '%s' '%s'", dest_path, url)
  return execute(cmd)
end

-- Verify SHA256 checksum
local function verify_sha256(file_path, expected_sha)
  if not file_path or file_path == "" then
    return nil, "file_path cannot be empty"
  end
  if not expected_sha or expected_sha == "" then
    return nil, "expected_sha cannot be empty"
  end

  local dir = file_path:match("^(.*)/[^/]+$") or "."
  local filename = file_path:match("([^/]+)$")
  local cmd = string.format("cd '%s' && echo '%s  %s' | shasum -a 256 -c",
    dir, expected_sha, filename)
  return execute(cmd)
end

-- Extract tar.gz archive
local function extract_targz(archive_path, output_dir, strip_components)
  local cmd = string.format("tar -xzf '%s' -C '%s' --strip-components=%d",
    archive_path, output_dir, strip_components)
  local ok, err = execute(cmd)
  if not ok then
    return nil, err
  end
  os.remove(archive_path)
  return true
end

-- Extract zip archive
local function extract_zip(archive_path, output_dir, strip_components)
  local cmd = string.format("unzip -q -DD -o '%s' -d '%s'", archive_path, output_dir)
  local ok, err = execute(cmd)
  if not ok then
    return nil, err
  end

  if strip_components == 1 then
    -- Move contents up one level: find the single top-level dir and move its contents
    local strip_cmd = string.format(
      "sh -c 'cd \"%s\" && " ..
      "dir=$(find . -mindepth 1 -maxdepth 1 -type d | head -1) && " ..
      "mv \"$dir\"/* \"$dir\"/.* . 2>/dev/null || true && " ..
      "rmdir \"$dir\" 2>/dev/null || true'",
      output_dir)
    local ok, err = execute(strip_cmd)
    if not ok then
      return nil, err
    end
  end

  os.remove(archive_path)
  return true
end

-- Extract gz compressed file
local function extract_gz(archive_path, tool_name, output_dir)
  local cmd = string.format("gunzip -f '%s'", archive_path)
  local ok, err = execute(cmd)
  if not ok then
    return nil, err
  end

  local binary_path = string.format("%s/%s", output_dir, tool_name)
  local chmod_cmd = string.format("/usr/bin/chmod +x '%s'", binary_path)
  return execute(chmod_cmd)
end

-- Make binary executable
local function make_executable(file_path)
  if not file_path or file_path == "" then
    return nil, "file_path cannot be empty"
  end

  local chmod_cmd = string.format("/usr/bin/chmod +x '%s'", file_path)
  return execute(chmod_cmd)
end

-- Extract based on format
local function extract(archive_path, output_dir, config)
  if config.format == "tar.gz" then
    return extract_targz(archive_path, output_dir, config.strip_components)
  elseif config.format == "zip" then
    return extract_zip(archive_path, output_dir, config.strip_components)
  elseif config.format == "gz" then
    return extract_gz(archive_path, config.tool_name, output_dir)
  elseif config.format == "binary" then
    return make_executable(archive_path)
  else
    return nil, "unknown format: " .. config.format
  end
end

-- Write file with error checking
local function write_file(path, content)
  local f, err = io.open(path, "w")
  if not f then
    return nil, "failed to open " .. path .. " for writing: " .. tostring(err)
  end
  f:write(content)
  f:close()
  return true
end

-- Write metadata files
local function write_metadata(output_dir, config)
  local ok, err = write_file(output_dir .. "/VERSION", config.version .. "\n")
  if not ok then
    return nil, err
  end

  local sha_short = config.sha:sub(1, 8)
  ok, err = write_file(output_dir .. "/SHA", sha_short .. "\n")
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
  local archive_path = output_dir .. "/" .. archive_name
  local cleanup_needed = false

  -- Transaction pattern: download and extract with cleanup on failure
  local ok, result = pcall(function()
    -- Download
    local ok, err = download_file(config.url, archive_path)
    if not ok then
      error(err)
    end
    cleanup_needed = true

    -- Verify checksum
    ok, err = verify_sha256(archive_path, config.sha)
    if not ok then
      error(err)
    end

    -- Extract
    ok, err = extract(archive_path, output_dir, config)
    if not ok then
      error(err)
    end
    cleanup_needed = false

    -- Write metadata
    ok, err = write_metadata(output_dir, config)
    if not ok then
      error(err)
    end

    return true
  end)

  -- Cleanup on failure
  if not ok and cleanup_needed then
    pcall(os.remove, archive_path)
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
