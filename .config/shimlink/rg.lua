local version = "14.1.1"
local repo = "BurntSushi/ripgrep"

local checksums = {
  ["darwin-arm64"] = "24ad76777745fbff131c8fbc466742b011f925bfa4fffa2ded6def23b5b937be",
  ["linux-arm64"] = "c827481c4ff4ea10c9dc7a4022c8de5db34a5737cb74484d62eb94a95841ab2f",
  ["linux-x86_64"] = "4cf9f2741e6c465ffdb7c26f38056a59e2a2544b51f7cc128ef28337eeae4d8e",
}

local platforms = {
  ["darwin-arm64"] = "aarch64-apple-darwin",
  ["linux-arm64"] = "aarch64-unknown-linux-gnu",
  ["linux-x86_64"] = "x86_64-unknown-linux-musl",
}

local urls = {}
for platform in pairs(checksums) do
  local triple = platforms[platform]
  local archive = string.format("ripgrep-%s-%s", version, triple)
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/%s.tar.gz", repo, version, archive)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
  path = "rg",
  strip_components = 1,
}
