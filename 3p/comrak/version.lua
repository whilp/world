return {
  version = "0.41.0",
  format = "binary",
  platforms = {
    ["darwin-arm64"] = {
      url = "https://github.com/kivikakk/comrak/releases/download/v${version}/comrak-${version}-aarch64-apple-darwin",
      sha = "ebff398559a48112e7699ad8ce8a35e1f5f0cf469ed44d55318b1d794abf1090",
    },
    ["linux-arm64"] = {
      url = "https://github.com/kivikakk/comrak/releases/download/v${version}/comrak-${version}-aarch64-unknown-linux-gnu",
      sha = "b76c1a02cd2b2d2b5f9dbde9d16124aa54d9e5a66fa2bc3f5f4d0ce637b1bb64",
    },
    ["linux-x86_64"] = {
      url = "https://github.com/kivikakk/comrak/releases/download/v${version}/comrak-${version}-x86_64-unknown-linux-gnu",
      sha = "d3ffc8f04f85a47fa325081affd6b572ad456b542a4d3a1207ef4685afd7e9e2",
    },
  },
}
