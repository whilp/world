-- box/claude.tl: install and configure Claude
local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local fetch = require("cosmic.fetch")

local backend = require("box.backend")

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

  local result = fetch.Fetch(url, {maxresponse = 300 * 1024 * 1024})
  if not result.ok then
    return false, "failed to download claude: " .. result.error
  end
  if result.status ~= 200 then
    return false, "failed to download claude: HTTP " .. tostring(result.status)
  end

  -- Verify SHA256
  local actual_sha = cosmo.EncodeHex(cosmo.Sha256(result.body)):lower()
  if actual_sha ~= platform_info.sha then
    return false, "claude checksum mismatch"
  end

  -- Write binary
  unix.makedirs(bin_dir)
  if not cosmo.Barf(claude_bin, result.body, tonumber("0755", 8)) then
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
      officialMarketplaceAutoInstallAttempted = true,
      officialMarketplaceAutoInstalled = true,
    }
    cosmo.Barf(claude_json, cosmo.EncodeJson(default_config) .. "\n", tonumber("0644", 8))
  end

  io.stderr:write("claude " .. version_info.version .. " installed\n")
  return true
end

local function write_file(filepath: string, content: string, mode: number): boolean, string
  local dir = path.dirname(filepath)
  if not unix.makedirs(dir) then
    return false, "failed to create directory: " .. dir
  end

  if not cosmo.Barf(filepath, content, mode or tonumber("0600", 8)) then
    return false, "failed to write: " .. filepath
  end
  return true
end

local function configure(env: backend.Env): boolean, string
  if not env.claude then
    return true -- no claude config
  end

  local home = os.getenv("HOME")
  if not home then
    return false, "HOME not set"
  end

  io.stderr:write("configuring claude...\n")

  local claude_dir = path.join(home, ".claude")
  unix.makedirs(claude_dir)

  -- Write credentials if provided (Lua table -> JSON)
  local credentials = env.claude["credentials"]
  if credentials then
    local creds_file = path.join(claude_dir, ".credentials.json")
    local json = cosmo.EncodeJson(credentials)
    local ok, err = write_file(creds_file, json, tonumber("0600", 8))
    if not ok then
      return false, "failed to write credentials: " .. (err or "unknown")
    end
  end

  -- Write settings to skip onboarding
  local settings_file = path.join(claude_dir, "settings.json")
  if not unix.stat(settings_file) then
    local settings = {
      numStartups = 1,
      installMethod = "unknown",
      autoUpdates = false,
      theme = "dark-daltonized",
      hasCompletedOnboarding = true,
      bypassPermissionsModeAccepted = true,
    }
    local ok, err = write_file(settings_file, cosmo.EncodeJson(settings), tonumber("0644", 8))
    if not ok then
      return false, "failed to write settings: " .. (err or "unknown")
    end
  end

  return true
end

return {
  install = install,
  configure = configure,
  detect_platform = detect_platform,
}
