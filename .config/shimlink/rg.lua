return {
  name = "rg",
  path = "${name}",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "24ad76777745fbff131c8fbc466742b011f925bfa4fffa2ded6def23b5b937be",
      arch = "aarch64-apple-darwin",
    },
    ["linux-arm64"] = {
      sha256 = "c827481c4ff4ea10c9dc7a4022c8de5db34a5737cb74484d62eb94a95841ab2f",
      arch = "aarch64-unknown-linux-gnu",
    },
    ["linux-x86_64"] = {
      sha256 = "4cf9f2741e6c465ffdb7c26f38056a59e2a2544b51f7cc128ef28337eeae4d8e",
      arch = "x86_64-unknown-linux-musl",
    },
  },
  repo = "BurntSushi/ripgrep",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${version}/ripgrep-${version}-${arch}.tar.gz",
  version = "14.1.1",
}
