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
      sha256 = "9412e11b0f8db9e1301ab4eda5ba49162c07a5b19edf9123aa3908cf65104380"
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha256 = "c1270a190a2df167c29095466991483c28b7fe71e85c6e55b97985c3aaccf93b"
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "2853c518d8ac68e22240f31c66951089247c65c36d2cfceb4a5b55baa1ffcb2e"
    }
  },
  release = "2025.11.29-b35f0e8",
  repo = "whilp/dotfiles",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${release}/${name}-${version}-${arch}.tar.gz",
  version = "2025.10.16-25a61a18"
}
