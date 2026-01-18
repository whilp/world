-- box/claude.tl: install Claude binary
local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local record ClaudeVersionPlatform
  sha: string
end

local record ClaudeVersion
  version: string
  base_url: string
  url: string
  platforms: {string: ClaudeVersionPlatform}
end

local function detect_platform(): string
  local os_name = cosmo.GetHostOs():lower()
  local isa = cosmo.GetHostIsa():lower()

  -- Normalize: XNU/OSX -> darwin
  if os_name == "xnu" or os_name == "osx" then
    os_name = "darwin"
  end

  -- Normalize arch to Claude's platform names
  if isa == "x86_64" then
    isa = "x64"
  end

  return os_name .. "-" .. isa
end

local function install(): boolean, string
  local ok, version_info = pcall(require, "claude.version") as (boolean, ClaudeVersion)
  if not ok then
    return false, "failed to load claude version info"
  end

  local platform = detect_platform()
  local platform_info = version_info.platforms[platform]
  if not platform_info then
    return false, "no platform info for " .. platform
  end

  -- Build URL from template
  local url = version_info.url
    :gsub("{base_url}", version_info.base_url)
    :gsub("{version}", version_info.version)
    :gsub("{platform}", platform)

  -- Install to ~/.local/share/claude/{version}-{sha8}/bin/claude
  local home = os.getenv("HOME")
  local short_sha = platform_info.sha:sub(1, 8)
  local bin_dir = path.join(home, ".local", "share", "claude",
    version_info.version .. "-" .. short_sha, "bin")
  local claude_bin = path.join(bin_dir, "claude")

  if unix.stat(claude_bin) then
    io.stderr:write("claude " .. version_info.version .. " already installed\n")
    return true
  end

  io.stderr:write("installing claude " .. version_info.version .. "...\n")

  local status, _, body = cosmo.Fetch(url, {maxresponse = 300 * 1024 * 1024})
  if not status or status ~= 200 then
    return false, "failed to download claude"
  end

  -- Verify SHA256
  local actual_sha = cosmo.EncodeHex(cosmo.Sha256(body)):lower()
  if actual_sha ~= platform_info.sha then
    return false, "claude checksum mismatch"
  end

  -- Write binary
  unix.makedirs(bin_dir)
  if not cosmo.Barf(claude_bin, body, tonumber("0755", 8)) then
    return false, "failed to write claude binary"
  end

  -- Create bin -> {version}-{sha}/bin symlink for stable path
  local share_dir = path.join(home, ".local", "share", "claude")
  local bin_link = path.join(share_dir, "bin")
  local link_target = version_info.version .. "-" .. short_sha .. "/bin"
  unix.unlink(bin_link)
  unix.symlink(link_target, bin_link)

  -- Create default ~/.claude.json if it doesn't exist
  local claude_json = path.join(home, ".claude.json")
  if not unix.stat(claude_json) then
    local default_config = {
      numStartups = 4,
      installMethod = "unknown",
      autoUpdates = true,
      theme = "dark-daltonized",
      hasCompletedOnboarding = true,
      lastOnboardingVersion = version_info.version,
      bypassPermissionsModeAccepted = true,
      lastReleaseNotesSeen = version_info.version,
    }
    cosmo.Barf(claude_json, cosmo.EncodeJson(default_config) .. "\n", tonumber("0644", 8))
  end

  io.stderr:write("claude " .. version_info.version .. " installed\n")
  return true
end

return {
  install = install,
  detect_platform = detect_platform,
}
