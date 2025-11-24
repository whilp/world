return {
  name = "nvim",
  path = "bin/${name}",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "877b95fe0d84aaaff51eab66c8c03c2bfc5202c572de6d5b10670159ab83cd2f",
      arch = "darwin-arm64",
    },
    ["linux-arm64"] = {
      sha256 = "9f1a2c06f9217a96878fbd494cd4198d57a47f651dc8f804b82acdb03c7fa607",
      arch = "linux-arm64",
    },
    ["linux-x86_64"] = {
      sha256 = "52ee34f0b4cf95c300716b21638f3c8aa4ec6a5761a1d77b44c5f3f4d73701f6",
      arch = "linux-x64",
    },
  },
  repo = "whilp/dotfiles",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${version}/${name}-${version}-${arch}.tar.gz",
  version = "2025.11.23",
}
