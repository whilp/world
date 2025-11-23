local version = "0.18.2"
local repo = "dandavison/delta"

local checksums = {
  ["darwin-arm64"] = "6ba38dce9f91ee1b9a24aa4aede1db7195258fe176c3f8276ae2d4457d8170a0",
  ["linux-arm64"] = "adf7674086daa4582f598f74ce9caa6b70c1ba8f4a57d2911499b37826b014f9",
  ["linux-x86_64"] = "b7ea845004762358a00ef9127dd9fd723e333c7e4b9cb1da220c3909372310ee",
}

local platforms = {
  ["darwin-arm64"] = "aarch64-apple-darwin",
  ["linux-arm64"] = "aarch64-unknown-linux-gnu",
  ["linux-x86_64"] = "x86_64-unknown-linux-musl",
}

local urls = {}
for platform in pairs(checksums) do
  local triple = platforms[platform]
  local archive = string.format("delta-%s-%s", version, triple)
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/%s.tar.gz", repo, version, archive)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
  path = "delta",
  strip_components = 1,
}
