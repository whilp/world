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
      sha256 = "30e0322cd6c93de00dd2c46a9ffce34c125d3f278b76577b40fd11b574dd921e"
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha256 = "bc678f36f3fdc47a8dd5bdad8bba20b54a1de21bd634b509267725be27d8c8c7"
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "464601d8923f93a3540730a5f73f902c71aeb07e77d578244b3037e461f69d8e"
    }
  },
  release = "2025.11.29-c7b10cb",
  repo = "whilp/dotfiles",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${release}/${name}-${version}-${arch}.tar.gz",
  version = "2025.10.16-25a61a18"
}
