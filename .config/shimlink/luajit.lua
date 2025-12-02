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
      sha256 = "b91254d85fa599a3de65b210e4c1bcb0799fb6e777fd8ed490cde55d12e7bfae"
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha256 = "53efb033dfca07672ab6dae56a3ee3bf99989bf806407ce9570d787967d74082"
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "60d43c1b20e75561ab338f7c91b3ed0945f58cc725ca16991c063bf66d8321c2"
    }
  },
  release = "2025.12.02-c993cc1",
  repo = "whilp/dotfiles",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${release}/${name}-${version}-${arch}.tar.gz",
  version = "2025.10.16-25a61a18"
}
