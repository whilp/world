local version = "cli%2Fv1.9.4"
local repo = "biomejs/biome"

local checksums = {
  ["darwin-arm64"] = "c68f2cbe09e9485426a749353a155b1d22c130c6ccdadc7772d603eb247b9a9d",
  ["linux-arm64"] = "f0f0f3e7cdec78420a600b05bfc364aa9b804811bd3bbae04e7bf090828ae970",
  ["linux-x86_64"] = "ce247fb644999ef52e5111dd6fd6e471019669fc9c4a44b5699721e39b7032c3",
}

local platforms = {
  ["darwin-arm64"] = "darwin-arm64",
  ["linux-arm64"] = "linux-arm64",
  ["linux-x86_64"] = "linux-x64",
}

local urls = {}
for platform in pairs(checksums) do
  local name = platforms[platform]
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/biome-%s", repo, version, name)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
}
