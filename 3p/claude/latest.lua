local cosmo = require("cosmo")
local spawn = require("spawn").spawn

local CLAUDE_BASE_URL = "https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819"

local function get_latest_version()
  local ok, output = spawn({"gh", "api", "repos/anthropics/claude-code/releases/latest", "--jq", ".tag_name"}):read()
  if not ok then
    io.stderr:write("error: failed to fetch latest version\n")
    return nil
  end
  local version = output:gsub("%s+$", "")
  if version then
    version = version:match("^v?(.+)$")
  end
  return version
end

local function get_sha256(url)
  local status, _, body = cosmo.Fetch(url, {maxresponse = 300 * 1024 * 1024})
  if not status then
    io.stderr:write("error: failed to download claude binary\n")
    return nil
  end
  if status ~= 200 then
    io.stderr:write("error: download failed with status " .. tostring(status) .. "\n")
    return nil
  end
  return cosmo.EncodeHex(cosmo.Sha256(body)):lower()
end

local function main()
  local version = get_latest_version()
  if not version then
    os.exit(1)
  end

  local url = string.format("%s/claude-code-releases/%s/linux-x64/claude", CLAUDE_BASE_URL, version)

  io.stderr:write("fetching sha256 for linux-x64...\n")
  local sha256 = get_sha256(url)
  if not sha256 then
    os.exit(1)
  end

  local result = {
    version = version,
    url = CLAUDE_BASE_URL .. "/claude-code-releases/{version}/{platform}/claude",
    platforms = {
      ["linux-x64"] = {sha = sha256},
    },
  }

  print("return " .. cosmo.EncodeLua(result, {pretty=true}))
end

main()
