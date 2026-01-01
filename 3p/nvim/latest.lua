#!/usr/bin/env lua
local cosmo = require("cosmo")

local repo = "whilp/neovim"
local api_url = "https://api.github.com/repos/" .. repo .. "/releases/latest"

local function fetch_json(url)
  local status, _, body = cosmo.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0", ["Accept"] = "application/vnd.github+json"},
  })
  if status ~= 200 then
    return nil, "fetch failed: " .. tostring(status)
  end
  return cosmo.DecodeJson(body)
end

local function fetch_sha256(url)
  local status, _, body = cosmo.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0"},
    maxresponse = 300 * 1024 * 1024,
  })
  if status ~= 200 then
    return nil, "fetch failed: " .. tostring(status)
  end
  return cosmo.EncodeHex(cosmo.Sha256(body)):lower()
end

local function get_commit_info(short_sha)
  local url = "https://api.github.com/repos/" .. repo .. "/commits/" .. short_sha
  local commit, err = fetch_json(url)
  if not commit then
    return nil, err
  end
  return commit.sha
end

local platform_map = {
  ["nvim-macos-arm64.tar.gz"] = "darwin-arm64",
  ["nvim-linux-arm64.tar.gz"] = "linux-arm64",
  ["nvim-linux-x86_64.tar.gz"] = "linux-x86_64",
}

local function main()
  io.stderr:write("fetching release info...\n")
  local release, err = fetch_json(api_url)
  if not release then
    io.stderr:write("error: " .. err .. "\n")
    os.exit(1)
  end

  local tag = release.tag_name
  local date, short_sha = tag:match("^(%d+%.%d+%.%d+)%-([0-9a-f]+)$")
  if not date or not short_sha then
    io.stderr:write("error: could not parse tag: " .. tag .. "\n")
    os.exit(1)
  end

  local assets = {}
  for _, asset in ipairs(release.assets) do
    local platform = platform_map[asset.name]
    if platform then
      assets[platform] = asset.browser_download_url
    end
  end

  io.stderr:write("fetching commit info...\n")
  local full_sha, sha_err = get_commit_info(short_sha)
  if not full_sha then
    io.stderr:write("error: " .. sha_err .. "\n")
    os.exit(1)
  end
  local version = "0.12.0-dev-" .. full_sha:sub(1, 10)

  local platforms = {}
  for platform, url in pairs(assets) do
    io.stderr:write("fetching sha256 for " .. platform .. "...\n")
    local sha, fetch_err = fetch_sha256(url)
    if not sha then
      io.stderr:write("error: " .. fetch_err .. "\n")
      os.exit(1)
    end
    local plat = {sha = sha}
    if platform == "darwin-arm64" then
      plat.platform = "macos-arm64"
    else
      plat.platform = platform
    end
    platforms[platform] = plat
  end

  local result = {
    version = version,
    date = date,
    tag = tag,
    format = "tar.gz",
    strip_components = 1,
    url = "https://github.com/" .. repo .. "/releases/download/{tag}/nvim-{platform}.tar.gz",
    platforms = platforms,
  }

  print("return " .. cosmo.EncodeLua(result, {pretty=true}))
end

main()
