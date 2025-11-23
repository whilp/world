local version = "2025.11.23"
local repo = "whilp/dotfiles"

local checksums = {
  ["darwin-arm64"] = "877b95fe0d84aaaff51eab66c8c03c2bfc5202c572de6d5b10670159ab83cd2f",
  ["linux-arm64"] = "9f1a2c06f9217a96878fbd494cd4198d57a47f651dc8f804b82acdb03c7fa607",
  ["linux-x86_64"] = "52ee34f0b4cf95c300716b21638f3c8aa4ec6a5761a1d77b44c5f3f4d73701f6",
}

local platforms = {
  ["darwin-arm64"] = "darwin-arm64",
  ["linux-arm64"] = "linux-arm64",
  ["linux-x86_64"] = "linux-x64",
}

local urls = {}
for platform in pairs(checksums) do
  local name = platforms[platform]
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/nvim-%s-%s.tar.gz", repo, version, version, name)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
  path = "bin/nvim",
  strip_components = 1,
}
