local version = "2025.10.16-25a61a18"
local release = "2025.11.23"
local repo = "whilp/dotfiles"

local checksums = {
  ["darwin-arm64"] = "651395c468e1890f92f3b54ae32a541c8340bcec7343de50227955b6f84bb2f7",
  ["linux-arm64"] = "fe01651e101c9fc237aba229e02ccaf058d13cfe0b8572382aee0ea1bef1c0d0",
  ["linux-x86_64"] = "9109f4c35137d9caf5eee168eeeccaa238d44b86ddf7dbb074c5ccea594d87d8",
}

local platforms = {
  ["darwin-arm64"] = "darwin-arm64",
  ["linux-arm64"] = "linux-arm64",
  ["linux-x86_64"] = "linux-x64",
}

local urls = {}
for platform in pairs(checksums) do
  local name = platforms[platform]
  urls[platform] = string.format("https://github.com/%s/releases/download/%s/luajit-%s-%s.tar.gz", repo, release, version, name)
end

return {
  version = version,
  repo = repo,
  checksums = checksums,
  urls = urls,
  strip_components = 1,
  binaries = {
    {
      name = "luajit",
      path = "bin/luajit",
      symlinks = {
        ["share/luajit-2.1"] = "~/.local/share/luajit-2.1",
      },
    },
    {
      name = "luarocks",
      path = "bin/luarocks",
    },
    {
      name = "luarocks-admin",
      path = "bin/luarocks-admin",
    },
  },
}
