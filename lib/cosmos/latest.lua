local cosmo = require("cosmo")

local REPO = "whilp/cosmopolitan"
local BINARIES = {"lua", "make", "unzip", "zip"}

local function get_latest_version()
  local url = "https://api.github.com/repos/" .. REPO .. "/releases"
  local status, _, body = cosmo.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0", ["Accept"] = "application/vnd.github+json"},
  })
  if not status then
    io.stderr:write("error: failed to fetch releases\n")
    return nil
  end
  if status ~= 200 then
    io.stderr:write("error: fetch failed with status " .. tostring(status) .. "\n")
    return nil
  end
  local releases = cosmo.DecodeJson(body)
  if not releases then
    io.stderr:write("error: invalid releases response\n")
    return nil
  end
  for _, release in ipairs(releases) do
    local tag = release.tag_name
    if tag and not tag:match("^cosmocc%-") then
      return tag
    end
  end
  io.stderr:write("error: no matching release found\n")
  return nil
end

local function get_sha256sums(version)
  local url = string.format("https://github.com/%s/releases/download/%s/SHA256SUMS", REPO, version)
  local status, _, body = cosmo.Fetch(url)
  if not status or status ~= 200 then
    return nil
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

local function main()
  local version = get_latest_version()
  if not version then
    os.exit(1)
  end

  io.stderr:write("fetching sha256sums for " .. version .. "...\n")
  local sums = get_sha256sums(version)
  if not sums then
    io.stderr:write("error: failed to fetch SHA256SUMS\n")
    os.exit(1)
  end

  local binaries = {}
  for _, name in ipairs(BINARIES) do
    if sums[name] then
      binaries[name] = sums[name]
    else
      io.stderr:write("warning: no sha256 for " .. name .. "\n")
    end
  end

  local result = {
    version = version,
    url = "https://github.com/" .. REPO .. "/releases/download/{version}/{binary}",
    binaries = binaries,
  }

  print("return " .. cosmo.EncodeLua(result, {pretty = true}))
end

main()
