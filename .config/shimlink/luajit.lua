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
      sha256 = "fec9c0554dcbdf267393c40f6788a4bbd1fa3d47722020256d2fe48e747ee5ac"
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha256 = "67d3c064f8beaa2ecffff638b11e2f881d02ecfb4f791621d3a95e9d67badd0a"
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "5c5579b64a342317b3000282afc20263f66d7a713870b67e2d196adcf6fad847"
    }
  },
  release = "2025.11.29-db85101",
  repo = "whilp/dotfiles",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${release}/${name}-${version}-${arch}.tar.gz",
  version = "2025.10.16-25a61a18"
}
