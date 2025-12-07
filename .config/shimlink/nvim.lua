Version{
  name = "nvim",
  path = "bin/${name}",
  platforms = {
    ["darwin-arm64"] = {
      arch = "darwin-arm64",
      sha256 = "143513b8f91dd29a510beef8c1202a9c623f5ced2f0f379df894d1d3e4b37039"
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha256 = "4a1101efbf237749c0727c356bc3dcf78be6fdbae27d63fc9a5d147b0808a821"
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "92b09500a845d5c5dd35473b28486c188a836ccc4fa3ab7fe54d2ce0777b4e0d"
    }
  },
  release = "2025.12.07-c016a4c",
  repo = "whilp/dotfiles",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${release}/${name}-${version}-${arch}.tar.gz",
  version = "2025.12.07"
}
