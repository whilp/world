local version = "v2.79.0"
local repo = "cli/cli"

local checksums = {
  ["darwin-arm64"] = "5454f9509e3dbb8f321310e9e344877d9a01ebb8f8703886b1afb0936d60ffaa",
  ["linux-arm64"] = "1b91e546b30181a8ee6d8c72bbf59eaadbb0600bab014dfbcc199676c83ea102",
  ["linux-x86_64"] = "e7af0c72a607c0528fda1989f7c8e3be85e67d321889002af0e2938ad9c8fb68",
}

local platforms = {
  ["darwin-arm64"] = { name = "macOS_arm64", ext = "zip" },
  ["linux-arm64"] = { name = "linux_arm64", ext = "tar.gz" },
  ["linux-x86_64"] = { name = "linux_amd64", ext = "tar.gz" },
}

local urls = {}
for platform in pairs(checksums) do
  local p = platforms[platform]
  local archive = string.format("gh_2.79.0_%s.%s", p.name, p.ext)
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/%s", repo, version, archive)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
  path = "bin/gh",
  strip_components = 1,
}
