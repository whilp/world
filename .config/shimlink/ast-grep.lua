local version = "0.28.0"
local repo = "ast-grep/ast-grep"

local checksums = {
  ["darwin-arm64"] = "c9a9e690d94cd9696d2552690fe0abdd2c303e48a3ee5cf9d38728eda054f147",
  ["linux-arm64"] = "62e9e79148be33d27fde24f4dcda83eab207a297ce50fb4a0becfbb29c8f218b",
  ["linux-x86_64"] = "d28be5970afb3e8022210fb9427de0875f1d64f4e4b91ed28b3a3abfebb1d934",
}

local platforms = {
  ["darwin-arm64"] = "aarch64-apple-darwin",
  ["linux-arm64"] = "aarch64-unknown-linux-gnu",
  ["linux-x86_64"] = "x86_64-unknown-linux-gnu",
}

local urls = {}
for platform in pairs(checksums) do
  local triple = platforms[platform]
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/app-%s.zip", repo, version, triple)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
  path = "ast-grep",
  strip_components = 0,
}
