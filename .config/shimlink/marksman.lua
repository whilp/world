local version = "2024-12-18"
local repo = "artempyanykh/marksman"

local checksums = {
  ["darwin-arm64"] = "7e18803966231a33ee107d0d26f69b41f2f0dc1332c52dd9729c2e29fb77be83",
  ["linux-arm64"] = "b8d6972a56f3f9b7bbbf7c77ef8998e3b66fa82fb03c01398e224144486c9e73",
  ["linux-x86_64"] = "b9cb666c643dfd9b699811fdfc445ed4c56be65c1d878c21d46847f0d7b0e475",
}

local platforms = {
  ["darwin-arm64"] = "macos",
  ["linux-arm64"] = "linux-arm64",
  ["linux-x86_64"] = "linux-x64",
}

local urls = {}
for platform in pairs(checksums) do
  local name = platforms[platform]
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/marksman-%s", repo, version, name)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
}
