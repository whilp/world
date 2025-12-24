#!/usr/bin/env lua
-- Fetches neovim nightly release for a specific platform
-- Usage: lua 3p/nvim/fetch-nightly.lua <platform> <output_dir>
-- Outputs: nvim-<platform>.tar.gz and nvim-<platform>.tar.gz.sha256

local cosmo = require("cosmo")
local path = cosmo.path
local unix = cosmo.unix
local spawn = require("spawn").spawn

local UPSTREAM_URL = "https://github.com/neovim/neovim/releases/download/nightly"

local PLATFORM_MAP = {
  ["darwin-arm64"] = "nvim-macos-arm64.tar.gz",
  ["linux-arm64"] = "nvim-linux-arm64.tar.gz",
  ["linux-x64"] = "nvim-linux-x86_64.tar.gz",
}

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

local function capture(program, args)
  local handle, err = spawn(args, { stdout = "pipe" })
  if not handle then
    return nil, string.format("command failed to start: %s (%s)", program, err or "unknown error")
  end
  local output = ""
  while true do
    local chunk = handle:read(4096)
    if not chunk or chunk == "" then break end
    output = output .. chunk
  end
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return nil, string.format("command failed: %s (exit: %d)", program, exit_code or -1)
  end
  return output:gsub("%s+$", "")
end

local function download_file(url, dest_path)
  return execute("/usr/bin/curl", {"curl", "-fsSL", "-o", dest_path, url})
end

local function calculate_sha256(file_path)
  local result, err = capture("/usr/bin/shasum", {"shasum", "-a", "256", file_path})
  if not result then
    return nil, err
  end
  return result:match("^(%S+)")
end

local function write_file(filepath, content)
  local fd = unix.open(filepath, unix.O_CREAT | unix.O_WRONLY | unix.O_TRUNC, 420)
  if not fd then
    return nil, "failed to open " .. filepath .. " for writing"
  end
  unix.write(fd, content)
  unix.close(fd)
  return true
end

local function fetch_nightly(platform, output_dir)
  if not platform or platform == "" then
    return nil, "platform is required"
  end
  if not output_dir or output_dir == "" then
    return nil, "output_dir is required"
  end

  local upstream_asset = PLATFORM_MAP[platform]
  if not upstream_asset then
    return nil, string.format("unknown platform: %s", platform)
  end

  output_dir = output_dir:gsub("/$", "")

  local upstream_url = UPSTREAM_URL .. "/" .. upstream_asset
  local output_name = "nvim-" .. platform .. ".tar.gz"
  local output_path = path.join(output_dir, output_name)

  io.write(string.format("fetching %s from %s\n", platform, upstream_url))

  local ok, err = download_file(upstream_url, output_path)
  if not ok then
    return nil, "download failed: " .. err
  end

  io.write("calculating sha256\n")
  local sha, sha_err = calculate_sha256(output_path)
  if not sha then
    return nil, "sha256 failed: " .. sha_err
  end

  local sha_content = sha .. "  " .. output_name .. "\n"
  local sha_path = output_path .. ".sha256"
  ok, err = write_file(sha_path, sha_content)
  if not ok then
    return nil, "failed to write sha256 file: " .. err
  end

  io.write(string.format("output: %s\n", output_path))
  io.write(string.format("sha256: %s\n", sha))

  return { path = output_path, sha = sha }
end

local M = {
  fetch_nightly = fetch_nightly,
  PLATFORM_MAP = PLATFORM_MAP,
}

if not pcall(debug.getlocal, 4, 1) then
  local platform = arg[1]
  local output_dir = arg[2] or "."

  local result, err = fetch_nightly(platform, output_dir)
  if not result then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
  os.exit(0)
end

return M
