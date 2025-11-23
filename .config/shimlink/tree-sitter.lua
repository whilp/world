local version = "v0.25.8"
local repo = "tree-sitter/tree-sitter"

local platforms = {
  ["darwin-arm64"] = "macos-arm64",
  ["linux-arm64"] = "linux-arm64",
  ["linux-x86_64"] = "linux-x64",
}

local checksums = {
  ["darwin-arm64"] = "ae3bbba3ba68e759a949e7591a42100a12d660cae165837aba48cae76a599e64",
  ["linux-arm64"] = "cd81d0108df9bdacf4fd32ec53534acced4780540eb5e889c77470d496e37fc5",
  ["linux-x86_64"] = "c9d46697e3e5ae6900a39ad4483667d2ba14c8ffb12c3f863bcf82a9564ee19f",
}

local urls = {}
for platform in pairs(checksums) do
  local name = platforms[platform]
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/tree-sitter-%s.gz", repo, version, name)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
}
