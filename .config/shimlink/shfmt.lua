local version = "v3.10.0"
local repo = "mvdan/sh"

local checksums = {
  ["darwin-arm64"] = "86030533a823c0a7cd92dee0f74094e5b901c3277b43def6337d5e19e56fe553",
  ["linux-arm64"] = "9d23013d56640e228732fd2a04a9ede0ab46bc2d764bf22a4a35fb1b14d707a8",
  ["linux-x86_64"] = "1f57a384d59542f8fac5f503da1f3ea44242f46dff969569e80b524d64b71dbc",
}

local platforms = {
  ["darwin-arm64"] = "darwin_arm64",
  ["linux-arm64"] = "linux_arm64",
  ["linux-x86_64"] = "linux_amd64",
}

local urls = {}
for platform in pairs(checksums) do
  local name = platforms[platform]
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/shfmt_%s_%s", repo, version, version, name)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
}
