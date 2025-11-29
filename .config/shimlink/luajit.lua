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
      sha256 = "448678e2ed7a2c5ed20b7a3e6c962b3de1a968c8e680bdeb1c256c639aa4a988"
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha256 = "c275e328e120bba4570cef3b199644e84ad8e87bf16546d987d1f41fc1ff0302"
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "7e12d8cfb9a79a4785ea58a1f26c166c36dd3d5576176083aa29ba59ddfae29b"
    }
  },
  release = "2025.11.29-943c2e9",
  repo = "whilp/dotfiles",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${release}/${name}-${version}-${arch}.tar.gz",
  version = "2025.10.16-25a61a18"
}
