local version = "2025.10.16-25a61a18"
local release = "2025.11.23"
local repo = "whilp/dotfiles"

local checksums = {
  ["darwin-arm64"] = "42d0565858a6300e3db14c230a215abad754974b356da4170373ac11c0eed5a6",
  ["linux-arm64"] = "b13bdf320a372672250f98b85aad56b003d0b679a7129d95ed54329b1503bbb5",
  ["linux-x86_64"] = "2de5d7bf1d65accfc1156bacd3035c97f4cfa99f8987302647a7d15b0347220e",
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
