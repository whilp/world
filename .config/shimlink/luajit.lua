Version{
  executables = {
    {
      name = "luajit",
      path = "bin/luajit",
      symlinks = {
        ["share/luajit-2.1"] = "~/.local/share/luajit-2.1"
      }
    },
    {
      name = "luarocks",
      path = "bin/luarocks"
    },
    {
      name = "luarocks-admin",
      path = "bin/luarocks-admin"
    },
    {
      name = "djot",
      path = "bin/djot"
    },
    {
      name = "lunamark",
      path = "bin/lunamark"
    }
  },
  name = "luajit",
  path = "bin/luajit",
  platforms = {
    ["darwin-arm64"] = {
      arch = "darwin-arm64",
      sha256 = "e78368409e80359133347646949b0a0acab5393a05d2a55ce6269de118e0cd8a"
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha256 = "ff677934539b8198b7d0950974e94a94336d25315364fa93fc350fec04a0ce03"
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "4f11d7085d003faf2f4bfc793a77d114266afb3bcd21c0b507ecfc3a4090d737"
    }
  },
  release = "2025.11.29-6f8e777",
  repo = "whilp/dotfiles",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${release}/${name}-${version}-${arch}.tar.gz",
  version = "2025.10.16-25a61a18"
}
