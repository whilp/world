local version = "v1.4.2"
local repo = "duckdb/duckdb"

local checksums = {
  ["darwin-arm64"] = "4dda25dff89b9757dd248f3a48c4d3e215dff64c4c9535a7822b3b7a7f4031c2",
  ["linux-arm64"] = "2b62c2fa4cb2f2e76e937b3b4baf20259cf6a5370e07ff310008ca9d5d6009c4",
  ["linux-x86_64"] = "fae3ba93eedf20b08bca4b23aeac1ba94c446f1c10d029c193e2fc4b4e0bc1bc",
}

local platforms = {
  ["darwin-arm64"] = "osx-arm64",
  ["linux-arm64"] = "linux-arm64",
  ["linux-x86_64"] = "linux-amd64",
}

local urls = {}
for platform in pairs(checksums) do
  local name = platforms[platform]
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/duckdb_cli-%s.zip", repo, version, name)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
  path = "duckdb",
}
