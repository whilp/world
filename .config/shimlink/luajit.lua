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
      sha256 = "7fb2f0aa28617906428c8cec1dc79913c7c799f1e844f16a170f9991f4066e21"
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha256 = "372fb15af7507cba3c2c797cec29a10a3367d5b4e0cfb25a4f567b1b1a9fdc41"
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "2e94bdffd0b4ce4d01897f23238460c80b6e8ba5ac3b86e2eb99f71c296e9198"
    }
  },
  release = "2025.11.29-09d63c4",
  repo = "whilp/dotfiles",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${release}/${name}-${version}-${arch}.tar.gz",
  version = "2025.10.16-25a61a18"
}
