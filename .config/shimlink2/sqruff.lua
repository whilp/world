return {
  name = "sqruff",
  path = "${name}",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "cb969b42ebbca8229b4484ae2503530c4eef16e23829b340a0b270e1a007e6b6",
      arch = "darwin-aarch64",
    },
    ["linux-arm64"] = {
      sha256 = "94ef0e55978a960f9cfc717bf5ed2127ae4462cc0a7915d7d38d843e3ca7ddfb",
      arch = "linux-aarch64-musl",
    },
    ["linux-x86_64"] = {
      sha256 = "ae09dfcb0d275bf5317769d6eff8aa62c05942369f63ea5e747164a7db9225d9",
      arch = "linux-x86_64-musl",
    },
  },
  repo = "quarylabs/sqruff",
  url = "https://github.com/${repo}/releases/download/${version}/${name}-${arch}.tar.gz",
  version = "v0.21.2",
}
