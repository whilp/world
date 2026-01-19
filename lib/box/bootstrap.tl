-- home/bootstrap.tl: download and unpack home from whilp/world releases
--
-- Detects platform, downloads home-<platform>, verifies SHA256, runs unpack.

local cosmo = require("cosmo")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")
local fetch = require("cosmic.fetch")

local REPO <const> = "whilp/world"
local API_URL <const> = "https://api.github.com"

local record ReleaseAsset
  name: string
  browser_download_url: string
end

local record Release
  tag_name: string
  assets: {ReleaseAsset}
end

local function detect_platform(): string, string
  local os_name = cosmo.GetHostOs()
  local isa = cosmo.GetHostIsa()

  if not os_name or not isa then
    return nil, "unable to detect platform"
  end

  -- normalize os: LINUX -> linux, XNU/OSX -> darwin
  os_name = os_name:lower()
  if os_name == "xnu" or os_name == "osx" then
    os_name = "darwin"
  end

  -- normalize arch: X86_64 -> x86_64, AARCH64 -> arm64
  isa = isa:lower()
  if isa == "aarch64" then
    isa = "arm64"
  end

  return os_name .. "-" .. isa
end

local function fetch_json(url: string): any, string
  local result = fetch.Fetch(url, {
    headers = {
      ["User-Agent"] = "curl/8.0",
      ["Accept"] = "application/vnd.github+json",
    },
  })
  if not result.ok then
    return nil, "fetch failed: " .. result.error
  end
  if result.status ~= 200 then
    return nil, "fetch failed: HTTP " .. tostring(result.status)
  end
  return cosmo.DecodeJson(result.body)
end

local function fetch_text(url: string): string, string
  local result = fetch.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0"},
  })
  if not result.ok then
    return nil, "fetch failed: " .. result.error
  end
  if result.status ~= 200 then
    return nil, "fetch failed: HTTP " .. tostring(result.status)
  end
  return result.body
end

local function fetch_binary(url: string): string, string
  local result = fetch.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0"},
    maxresponse = 250 * 1024 * 1024,
  })
  if not result.ok then
    return nil, "fetch failed: " .. result.error
  end
  if result.status ~= 200 then
    return nil, "fetch failed: HTTP " .. tostring(result.status)
  end
  return result.body
end

local function find_asset(release: Release, name: string): ReleaseAsset
  for _, asset in ipairs(release.assets) do
    if asset.name == name then
      return asset
    end
  end
  return nil
end

local function parse_sha256sums(content: string): {string:string}
  local sums: {string:string} = {}
  for line in content:gmatch("[^\n]+") do
    local sha, name = line:match("^(%x+)%s+(.+)$")
    if sha and name then
      sums[name] = sha
    end
  end
  return sums
end

local record BootstrapConfig
  repo: string
  release: string
end

local function load_bootstrap_config(): BootstrapConfig
  local config: BootstrapConfig = {}

  -- Read from environment variables (set by box.run)
  local repo = os.getenv("BOOTSTRAP_REPO")
  local release = os.getenv("BOOTSTRAP_RELEASE")

  if repo and repo ~= "" then
    config.repo = repo
  end
  if release and release ~= "" then
    config.release = release
  end

  return config
end

local function get_release(): Release, string
  local config = load_bootstrap_config()
  local repo = config.repo or REPO
  local release_tag = config.release
  local url: string

  if release_tag and release_tag ~= "" then
    io.stderr:write("fetching release " .. release_tag .. " from " .. repo .. "...\n")
    url = API_URL .. "/repos/" .. repo .. "/releases/tags/" .. release_tag
  else
    io.stderr:write("fetching latest release from " .. repo .. "...\n")
    url = API_URL .. "/repos/" .. repo .. "/releases/latest"
  end

  local data, err = fetch_json(url)
  if not data then
    return nil, err
  end
  local release = data as Release
  io.stderr:write("release: " .. release.tag_name .. "\n")
  return release
end

local function get_checksums(release: Release): {string:string}, string
  local sums_asset = find_asset(release, "SHA256SUMS")
  if not sums_asset then
    return nil, "SHA256SUMS not found in release"
  end

  io.stderr:write("fetching SHA256SUMS...\n")
  local content, err = fetch_text(sums_asset.browser_download_url)
  if not content then
    return nil, err
  end

  return parse_sha256sums(content)
end

local function download_home(release: Release, platform: string, expected_sha: string): string, string
  local asset_name = "home-" .. platform
  local asset = find_asset(release, asset_name)
  if not asset then
    return nil, asset_name .. " not found in release"
  end

  io.stderr:write("downloading " .. asset_name .. " from " .. asset.browser_download_url .. "\n")
  local body, err = fetch_binary(asset.browser_download_url)
  if not body then
    return nil, err
  end
  io.stderr:write("downloaded " .. tostring(#body) .. " bytes\n")

  local actual_sha = cosmo.EncodeHex(cosmo.Sha256(body)):lower()
  if actual_sha ~= expected_sha then
    return nil, "SHA256 mismatch: expected " .. expected_sha .. ", got " .. actual_sha
  end
  io.stderr:write("SHA256 verified\n")

  return body
end

local function write_binary(data: string, dest: string): boolean, string
  if not cosmo.Barf(dest, data, tonumber("0755", 8)) then
    return false, "failed to write " .. dest
  end
  return true
end

local function run_unpack(home_bin: string, home_dir: string): boolean, string
  io.stderr:write("unpacking to " .. home_dir .. "...\n")
  local exit_code = spawn({home_bin, "unpack", "--force", home_dir}):wait()
  if exit_code ~= 0 then
    return false, "unpack failed with exit code " .. tostring(exit_code)
  end
  return true
end

local function main(): integer, string
  local home_dir = os.getenv("HOME")
  if not home_dir then
    return 1, "HOME not set"
  end

  local platform, err = detect_platform()
  if not platform then
    return 1, err
  end
  io.stderr:write("platform: " .. platform .. "\n")

  local release: Release
  release, err = get_release()
  if not release then
    return 1, err
  end

  local sums: {string:string}
  sums, err = get_checksums(release)
  if not sums then
    return 1, err
  end

  local asset_name = "home-" .. platform
  local expected_sha = sums[asset_name]
  if not expected_sha then
    return 1, "no SHA256 for " .. asset_name
  end

  local body: string
  body, err = download_home(release, platform, expected_sha)
  if not body then
    return 1, err
  end

  local tmp_dir = os.getenv("TMPDIR") or "/tmp"
  local home_bin = path.join(tmp_dir, "home-" .. release.tag_name)

  local ok: boolean
  ok, err = write_binary(body, home_bin)
  if not ok then
    return 1, err
  end

  ok, err = run_unpack(home_bin, home_dir)
  if not ok then
    return 1, err
  end

  io.stderr:write("bootstrap complete\n")
  return 0
end

if cosmo.is_main() then
  local code, err = main()
  if err then
    io.stderr:write("error: " .. err .. "\n")
  end
  os.exit(code)
end

return {
  detect_platform = detect_platform,
  main = main,
}
