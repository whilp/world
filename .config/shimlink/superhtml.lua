local version = "v0.5.3"
local repo = "kristoff-it/superhtml"

local checksums = {
  ["darwin-arm64"] = "b8b2327f666ff316422061284e107add5c413ebdfdb91774c0c3702a66e65ec9",
  ["linux-arm64"] = "54cd2414de6664b85166a0a2e7c208ca3dbcc935274f4a55309cc9dcfa8e605b",
  ["linux-x86_64"] = "c9fabbbd57851e38a67e6c1eb7942e8bc6189925bfcf437f1e5286932c76d60a",
}

local platforms = {
  ["darwin-arm64"] = "aarch64-macos",
  ["linux-arm64"] = "aarch64-linux",
  ["linux-x86_64"] = "x86_64-linux-musl",
}

local urls = {}
for platform in pairs(checksums) do
  local target = platforms[platform]
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/%s.tar.gz", repo, version, target)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
  path = "superhtml",
  strip_components = 1,
}
