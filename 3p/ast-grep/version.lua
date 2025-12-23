return {
  version = "0.28.0",
  format = "zip",
  strip_components = 0,
  platforms = {
    ["darwin-arm64"] = {
      url = "https://github.com/ast-grep/ast-grep/releases/download/${version}/app-aarch64-apple-darwin.zip",
      sha = "c9a9e690d94cd9696d2552690fe0abdd2c303e48a3ee5cf9d38728eda054f147",
    },
    ["linux-arm64"] = {
      url = "https://github.com/ast-grep/ast-grep/releases/download/${version}/app-aarch64-unknown-linux-gnu.zip",
      sha = "62e9e79148be33d27fde24f4dcda83eab207a297ce50fb4a0becfbb29c8f218b",
    },
    ["linux-x86_64"] = {
      url = "https://github.com/ast-grep/ast-grep/releases/download/${version}/app-x86_64-unknown-linux-gnu.zip",
      sha = "d28be5970afb3e8022210fb9427de0875f1d64f4e4b91ed28b3a3abfebb1d934",
    },
  },
}
