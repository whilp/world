local version = "v0.21.2"
local repo = "quarylabs/sqruff"

local checksums = {
  ["darwin-arm64"] = "cb969b42ebbca8229b4484ae2503530c4eef16e23829b340a0b270e1a007e6b6",
  ["linux-arm64"] = "94ef0e55978a960f9cfc717bf5ed2127ae4462cc0a7915d7d38d843e3ca7ddfb",
  ["linux-x86_64"] = "ae09dfcb0d275bf5317769d6eff8aa62c05942369f63ea5e747164a7db9225d9",
}

local platforms = {
  ["darwin-arm64"] = "darwin-aarch64",
  ["linux-arm64"] = "linux-aarch64-musl",
  ["linux-x86_64"] = "linux-x86_64-musl",
}

local urls = {}
for platform in pairs(checksums) do
  local target = platforms[platform]
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/sqruff-%s.tar.gz", repo, version, target)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
  path = "sqruff",
}
