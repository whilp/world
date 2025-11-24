return {
  name = "ast-grep",
  path = "${name}",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "c9a9e690d94cd9696d2552690fe0abdd2c303e48a3ee5cf9d38728eda054f147",
      arch = "aarch64-apple-darwin",
    },
    ["linux-arm64"] = {
      sha256 = "62e9e79148be33d27fde24f4dcda83eab207a297ce50fb4a0becfbb29c8f218b",
      arch = "aarch64-unknown-linux-gnu",
    },
    ["linux-x86_64"] = {
      sha256 = "d28be5970afb3e8022210fb9427de0875f1d64f4e4b91ed28b3a3abfebb1d934",
      arch = "x86_64-unknown-linux-gnu",
    },
  },
  repo = "ast-grep/ast-grep",
  strip_components = 0,
  url = "https://github.com/${repo}/releases/download/${version}/app-${arch}.zip",
  version = "0.28.0",
}
