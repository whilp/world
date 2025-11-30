Version{
  name = "nvim",
  path = "bin/${name}",
  platforms = {
    ["darwin-arm64"] = {
      arch = "darwin-arm64",
      sha256 = "877b95fe0d84aaaff51eab66c8c03c2bfc5202c572de6d5b10670159ab83cd2f"
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha256 = "04f38df6f95c702eb9368d9b64fc04ff74f8027a61ca00181e0100e66fcb75b5"
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "9d6fc63c2a0b96d11d435dfadab58042dfab86c7cbb394dc6cd7f3d0f004b61a"
    }
  },
  repo = "whilp/dotfiles",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${version}/${name}-${version}-${arch}.tar.gz",
  version = "2025.11.23"
}
