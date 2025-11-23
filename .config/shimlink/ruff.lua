local version = "0.8.4"
local repo = "astral-sh/ruff"

local checksums = {
  ["darwin-arm64"] = "8893f3ede33a73740f69b10ee9356e5cf2933c0afe146f00176be12ef91bf9d9",
  ["linux-arm64"] = "0dfe36fabb817638863375e0140ce03bf26ccc9a7fd9d2c8e8337b1a21697ed4",
  ["linux-x86_64"] = "c4e6591ae1bb4f15c09c9022b7bfc57e1c3a567acdc9cd76021cd1304b5868c3",
}

local platforms = {
  ["darwin-arm64"] = "aarch64-apple-darwin",
  ["linux-arm64"] = "aarch64-unknown-linux-gnu",
  ["linux-x86_64"] = "x86_64-unknown-linux-gnu",
}

local urls = {}
for platform in pairs(checksums) do
  local triple = platforms[platform]
  local archive = string.format("ruff-%s", triple)
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/%s.tar.gz", repo, version, archive)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
  path = "ruff",
  strip_components = 1,
}
