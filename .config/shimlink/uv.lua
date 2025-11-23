local version = "0.5.7"
local repo = "astral-sh/uv"

local checksums = {
  ["darwin-arm64"] = "b8cab25ab2ec0714dbb34179f948c27aa4ab307be54e0628e9e1eef1d2264f9f",
  ["linux-arm64"] = "d4dd7a72689888c92b5191902fd4ec9d25b7eeba07be41ba4a8f89acbb403e2d",
  ["linux-x86_64"] = "8a0a3e823684dec6e49ae17f31bf6483c778fd579671992d9156875210e5161e",
}

local platforms = {
  ["darwin-arm64"] = "aarch64-apple-darwin",
  ["linux-arm64"] = "aarch64-unknown-linux-gnu",
  ["linux-x86_64"] = "x86_64-unknown-linux-gnu",
}

local urls = {}
for platform in pairs(checksums) do
  local triple = platforms[platform]
  local archive = string.format("uv-%s", triple)
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/%s.tar.gz", repo, version, archive)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
  path = "uv",
  strip_components = 1,
}
