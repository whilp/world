return {
  version = "0.21.2",
  format = "tar.gz",
  strip_components = 0,
  platforms = {
    ["darwin-arm64"] = {
      url = "https://github.com/quarylabs/sqruff/releases/download/v${version}/sqruff-darwin-aarch64.tar.gz",
      sha = "cb969b42ebbca8229b4484ae2503530c4eef16e23829b340a0b270e1a007e6b6",
    },
    ["linux-arm64"] = {
      url = "https://github.com/quarylabs/sqruff/releases/download/v${version}/sqruff-linux-aarch64-musl.tar.gz",
      sha = "94ef0e55978a960f9cfc717bf5ed2127ae4462cc0a7915d7d38d843e3ca7ddfb",
    },
    ["linux-x86_64"] = {
      url = "https://github.com/quarylabs/sqruff/releases/download/v${version}/sqruff-linux-x86_64-musl.tar.gz",
      sha = "ae09dfcb0d275bf5317769d6eff8aa62c05942369f63ea5e747164a7db9225d9",
    },
  },
}
