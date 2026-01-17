-- home/bootstrap.tl: download and unpack home from whilp/world releases
--
-- Detects platform, downloads home-<platform>, verifies SHA256, runs unpack.

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

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

  -- normalize os: LINUX -> linux, OSX -> darwin
  os_name = os_name:lower()
  if os_name == "osx" then
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
  local status, _, body = cosmo.Fetch(url, {
    headers = {
      ["User-Agent"] = "curl/8.0",
      ["Accept"] = "application/vnd.github+json",
    },
  })
  if not status or status ~= 200 then
    return nil, "fetch failed: " .. tostring(status)
  end
  return cosmo.DecodeJson(body)
end

local function fetch_text(url: string): string, string
  local status, _, body = cosmo.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0"},
  })
  if not status or status ~= 200 then
    return nil, "fetch failed: " .. tostring(status)
  end
  return body
end

local function fetch_binary(url: string): string, string
  local status, _, body = cosmo.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0"},
    maxresponse = 200 * 1024 * 1024,
  })
  if not status or status ~= 200 then
    return nil, "fetch failed: " .. tostring(status)
  end
  return body
end

local function get_latest_release(): Release, string
  local url = API_URL .. "/repos/" .. REPO .. "/releases/latest"
  local data, err = fetch_json(url)
  if not data then
    return nil, err
  end
  return data as Release
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

local function find_asset(release: Release, name: string): ReleaseAsset
  for _, asset in ipairs(release.assets) do
    if asset.name == name then
      return asset
    end
  end
  return nil
end

local function main(): number
  local home_dir = os.getenv("HOME")
  if not home_dir then
    io.stderr:write("error: HOME not set\n")
    return 1
  end

  -- detect platform
  local platform, err = detect_platform()
  if not platform then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end
  io.stderr:write("platform: " .. platform .. "\n")

  -- get latest release
  io.stderr:write("fetching latest release...\n")
  local release: Release
  release, err = get_latest_release()
  if not release then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end
  io.stderr:write("release: " .. release.tag_name .. "\n")

  -- download SHA256SUMS
  local sums_asset = find_asset(release, "SHA256SUMS")
  if not sums_asset then
    io.stderr:write("error: SHA256SUMS not found in release\n")
    return 1
  end

  io.stderr:write("fetching SHA256SUMS...\n")
  local sums_content: string
  sums_content, err = fetch_text(sums_asset.browser_download_url)
  if not sums_content then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end

  local sums = parse_sha256sums(sums_content)

  -- find home asset for platform
  local asset_name = "home-" .. platform
  local expected_sha = sums[asset_name]
  if not expected_sha then
    io.stderr:write("error: no SHA256 for " .. asset_name .. "\n")
    io.stderr:write("available assets:\n")
    for name, _ in pairs(sums) do
      io.stderr:write("  " .. name .. "\n")
    end
    return 1
  end

  local home_asset = find_asset(release, asset_name)
  if not home_asset then
    io.stderr:write("error: " .. asset_name .. " not found in release\n")
    return 1
  end

  -- download home binary
  io.stderr:write("downloading " .. asset_name .. "...\n")
  local body: string
  body, err = fetch_binary(home_asset.browser_download_url)
  if not body then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end
  io.stderr:write("downloaded " .. tostring(#body) .. " bytes\n")

  -- verify SHA256
  local actual_sha = cosmo.EncodeHex(cosmo.Sha256(body)):lower()
  if actual_sha ~= expected_sha then
    io.stderr:write("error: SHA256 mismatch\n")
    io.stderr:write("  expected: " .. expected_sha .. "\n")
    io.stderr:write("  actual:   " .. actual_sha .. "\n")
    return 1
  end
  io.stderr:write("SHA256 verified\n")

  -- write to temp file
  local tmp_dir = os.getenv("TMPDIR") or "/tmp"
  local home_bin = path.join(tmp_dir, "home-" .. release.tag_name)

  local fd = unix.open(home_bin, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("0755", 8))
  if not fd or fd < 0 then
    io.stderr:write("error: failed to create " .. home_bin .. "\n")
    return 1
  end
  unix.write(fd, body)
  unix.close(fd)

  -- run unpack
  io.stderr:write("unpacking to " .. home_dir .. "...\n")
  local exit_code = spawn({home_bin, "unpack", "--force", home_dir}):wait()
  if exit_code ~= 0 then
    io.stderr:write("error: unpack failed with exit code " .. tostring(exit_code) .. "\n")
    return 1
  end

  io.stderr:write("bootstrap complete\n")
  return 0
end

if cosmo.is_main() then
  os.exit(main())
end

return {
  detect_platform = detect_platform,
  main = main,
}
