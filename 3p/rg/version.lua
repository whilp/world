return {
  version = "14.1.1",
  format = "tar.gz",
  strip_components = 1,
  platforms = {
    ["darwin-arm64"] = {
      url = "https://github.com/BurntSushi/ripgrep/releases/download/${version}/ripgrep-${version}-aarch64-apple-darwin.tar.gz",
      sha = "24ad76777745fbff131c8fbc466742b011f925bfa4fffa2ded6def23b5b937be",
    },
    ["linux-arm64"] = {
      url = "https://github.com/BurntSushi/ripgrep/releases/download/${version}/ripgrep-${version}-aarch64-unknown-linux-gnu.tar.gz",
      sha = "c827481c4ff4ea10c9dc7a4022c8de5db34a5737cb74484d62eb94a95841ab2f",
    },
    ["linux-x86_64"] = {
      url = "https://github.com/BurntSushi/ripgrep/releases/download/${version}/ripgrep-${version}-x86_64-unknown-linux-musl.tar.gz",
      sha = "4cf9f2741e6c465ffdb7c26f38056a59e2a2544b51f7cc128ef28337eeae4d8e",
    },
  },
}
