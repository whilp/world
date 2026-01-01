#!/usr/bin/env lua
local cosmo = require("cosmo")

local REPO = "whilp/cosmopolitan"

local function get_latest_version()
  local url = "https://api.github.com/repos/" .. REPO .. "/releases/latest"
  local status, _, body = cosmo.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0", ["Accept"] = "application/vnd.github+json"},
  })
  if not status then
    return nil, "failed to fetch releases"
  end
  if status ~= 200 then
    return nil, "fetch failed with status " .. tostring(status)
  end
  local release = cosmo.DecodeJson(body)
  if not release or not release.tag_name then
    return nil, "invalid release response"
  end
  return release.tag_name
end

local function get_sha256sums(version)
  local url = string.format("https://github.com/%s/releases/download/%s/SHA256SUMS", REPO, version)
  local status, _, body = cosmo.Fetch(url)
  if not status or status ~= 200 then
    return nil, "failed to fetch SHA256SUMS"
  end
  local sums = {}
  for line in body:gmatch("[^\n]+") do
    local sha, name = line:match("^(%x+)%s+(.+)$")
    if sha and name then
      sums[name] = sha:lower()
    end
  end
  return sums
end

local function main(opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr

  local version, err = get_latest_version()
  if not version then
    return 1, err
  end

  stderr:write("fetching sha256sums for " .. version .. "...\n")
  local sums, sums_err = get_sha256sums(version)
  if not sums then
    return 1, sums_err
  end

  local sha = sums["cosmos.zip"]
  if not sha then
    return 1, "no sha256 for cosmos.zip"
  end

  local result = {
    version = version,
    format = "zip",
    url = "https://github.com/" .. REPO .. "/releases/download/{version}/cosmos.zip",
    platforms = {["*"] = {sha = sha}},
  }

  print("return " .. cosmo.EncodeLua(result, {pretty = true}))
  return 0
end

local status, err = main()
if err then
  io.stderr:write("error: " .. err .. "\n")
end
os.exit(status)
