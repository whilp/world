local version = "v2.0.1"
local repo = "JohnnyMorganz/StyLua"

local checksums = {
  ["darwin-arm64"] = "3d9caaa660da4b3bc092e805d09af59e42b7504f1253c863b682ea3fc80944f2",
  ["linux-arm64"] = "3db53cd00a685d0b59f4a4ab188bfa6acb804dca489d810a852ed2ea32eb2b1c",
  ["linux-x86_64"] = "9087e42f599855192cf4f6a7fb0cb7353e23debd7c749c6e3a76fc58abde3c89",
}

local platforms = {
  ["darwin-arm64"] = "macos-aarch64",
  ["linux-arm64"] = "linux-aarch64",
  ["linux-x86_64"] = "linux-x86_64",
}

local urls = {}
for platform in pairs(checksums) do
  local target = platforms[platform]
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/stylua-%s.zip", repo, version, target)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
  path = "stylua",
}
