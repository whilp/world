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
      sha256 = "20e2f8496582adaec9340471147320b2f536c8da4c28075f3c9949de77860835"
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha256 = "36b2e28e6bb98c62f1f85206753be9cd4a4fab944f1dbd6b41727f12ccdbc3c3"
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "b235677b901f8ef71bde222506aa632d35c015b01400e73c24b44306ad3fab40"
    }
  },
  release = "2025.12.06-06f06e0",
  repo = "whilp/dotfiles",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${release}/${name}-${version}-${arch}.tar.gz",
  version = "2025.10.16-25a61a18"
}
