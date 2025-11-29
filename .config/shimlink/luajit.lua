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
      sha256 = "deb7d5ad79754bd92201208814be74359eb2478a04b73d3ac831ab1219828df7"
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha256 = "e81185a41db82aa9f57a5b7438b5ef05ae6ee44a7902c1b26b65274375c37f92"
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "e0bfaf1cf5d374ad71fbd5e0cf31d8303fcf04c532b8b815fdf78ff7dd339b7a"
    }
  },
  release = "2025.11.29-2ed7477",
  repo = "whilp/dotfiles",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${release}/${name}-${version}-${arch}.tar.gz",
  version = "2025.10.16-25a61a18"
}
