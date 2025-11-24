return {
  name = "luajit",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "42d0565858a6300e3db14c230a215abad754974b356da4170373ac11c0eed5a6",
      arch = "darwin-arm64",
    },
    ["linux-arm64"] = {
      sha256 = "b13bdf320a372672250f98b85aad56b003d0b679a7129d95ed54329b1503bbb5",
      arch = "linux-arm64",
    },
    ["linux-x86_64"] = {
      sha256 = "2de5d7bf1d65accfc1156bacd3035c97f4cfa99f8987302647a7d15b0347220e",
      arch = "linux-x64",
    },
  },
  repo = "whilp/dotfiles",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${release}/${name}-${version}-${arch}.tar.gz",
  version = "2025.10.16-25a61a18",
  release = "2025.11.23",
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
