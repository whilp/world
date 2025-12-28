local cosmo = require("cosmo")

local CLAUDE_BASE_URL = "https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819"

local function get_latest_version()
  local url = "https://api.github.com/repos/anthropics/claude-code/releases/latest"
  local status, _, body = cosmo.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0", ["Accept"] = "application/vnd.github+json"},
  })
  if not status then
    io.stderr:write("error: failed to fetch release info\n")
    return nil
  end
  if status ~= 200 then
    io.stderr:write("error: fetch failed with status " .. tostring(status) .. "\n")
    return nil
  end
  local release = cosmo.DecodeJson(body)
  if not release or not release.tag_name then
    io.stderr:write("error: invalid release response\n")
    return nil
  end
  local version = release.tag_name:match("^v?(.+)$")
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
    base_url = CLAUDE_BASE_URL,
    url = "{base_url}/claude-code-releases/{version}/{platform}/claude",
    platforms = {
      ["linux-x64"] = {sha = sha256},
    },
  }

  print("return " .. cosmo.EncodeLua(result, {pretty = true}))
end

main()
