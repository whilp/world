return {
  version = "14.1.1",
  format = "tar.gz",
  strip_components = 1,
  url = "https://github.com/BurntSushi/ripgrep/releases/download/{version}/ripgrep-{version}-{arch}.tar.gz",
  platforms = {
    ["darwin-arm64"] = {
      arch = "aarch64-apple-darwin",
      sha = "24ad76777745fbff131c8fbc466742b011f925bfa4fffa2ded6def23b5b937be",
    },
    ["linux-arm64"] = {
      arch = "aarch64-unknown-linux-gnu",
      sha = "c827481c4ff4ea10c9dc7a4022c8de5db34a5737cb74484d62eb94a95841ab2f",
    },
    ["linux-x86_64"] = {
      arch = "x86_64-unknown-linux-musl",
      sha = "4cf9f2741e6c465ffdb7c26f38056a59e2a2544b51f7cc128ef28337eeae4d8e",
    },
  },
}
