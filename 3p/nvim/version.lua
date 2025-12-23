return {
  version = "2025.12.07",
  release_sha = "c016a4c",
  format = "tar.gz",
  strip_components = 1,
  platforms = {
    ["darwin-arm64"] = {
      url = "https://github.com/whilp/dotfiles/releases/download/${version}-${release_sha}/nvim-${version}-darwin-arm64.tar.gz",
      sha = "143513b8f91dd29a510beef8c1202a9c623f5ced2f0f379df894d1d3e4b37039",
    },
    ["linux-arm64"] = {
      url = "https://github.com/whilp/dotfiles/releases/download/${version}-${release_sha}/nvim-${version}-linux-arm64.tar.gz",
      sha = "4a1101efbf237749c0727c356bc3dcf78be6fdbae27d63fc9a5d147b0808a821",
    },
    ["linux-x86_64"] = {
      url = "https://github.com/whilp/dotfiles/releases/download/${version}-${release_sha}/nvim-${version}-linux-x64.tar.gz",
      sha = "92b09500a845d5c5dd35473b28486c188a836ccc4fa3ab7fe54d2ce0777b4e0d",
    },
  },
}
