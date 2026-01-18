return {
  version = "15.1.0",
  format = "tar.gz",
  strip_components = 1,
  url = "https://github.com/BurntSushi/ripgrep/releases/download/{version}/ripgrep-{version}-{arch}.tar.gz",
  platforms = {
    ["darwin-arm64"] = {
      arch = "aarch64-apple-darwin",
      sha = "378e973289176ca0c6054054ee7f631a065874a352bf43f0fa60ef079b6ba715",
    },
    ["linux-arm64"] = {
      arch = "aarch64-unknown-linux-gnu",
      sha = "2b661c6ef508e902f388e9098d9c4c5aca72c87b55922d94abdba830b4dc885e",
    },
    ["linux-x86_64"] = {
      arch = "x86_64-unknown-linux-musl",
      sha = "1c9297be4a084eea7ecaedf93eb03d058d6faae29bbc57ecdaf5063921491599",
    },
  },
}
