local version = "v0.41.0"
local repo = "kivikakk/comrak"

local checksums = {
  ["darwin-arm64"] = "ebff398559a48112e7699ad8ce8a35e1f5f0cf469ed44d55318b1d794abf1090",
  ["linux-arm64"] = "b76c1a02cd2b2d2b5f9dbde9d16124aa54d9e5a66fa2bc3f5f4d0ce637b1bb64",
  ["linux-x86_64"] = "d3ffc8f04f85a47fa325081affd6b572ad456b542a4d3a1207ef4685afd7e9e2",
}

local platforms = {
  ["darwin-arm64"] = "aarch64-apple-darwin",
  ["linux-arm64"] = "aarch64-unknown-linux-gnu",
  ["linux-x86_64"] = "x86_64-unknown-linux-musl",
}

local exec_config = {
  ["linux-arm64"] = { "/lib/ld-linux-aarch64.so.1", "{binary}" },
  ["linux-x86_64"] = { "/lib64/ld-linux-x86-64.so.2", "{binary}" },
}

local urls = {}
for platform in pairs(checksums) do
  local triple = platforms[platform]
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/comrak-0.41.0-%s", repo, version, triple)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
  exec = exec_config,
}
