return {
  name = "comrak",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "ebff398559a48112e7699ad8ce8a35e1f5f0cf469ed44d55318b1d794abf1090",
      arch = "aarch64-apple-darwin",
    },
    ["linux-arm64"] = {
      sha256 = "b76c1a02cd2b2d2b5f9dbde9d16124aa54d9e5a66fa2bc3f5f4d0ce637b1bb64",
      arch = "aarch64-unknown-linux-gnu",
      exec = { "/lib/ld-linux-aarch64.so.1", "${destination}/${name}" },
    },
    ["linux-x86_64"] = {
      sha256 = "d3ffc8f04f85a47fa325081affd6b572ad456b542a4d3a1207ef4685afd7e9e2",
      arch = "x86_64-unknown-linux-musl",
      exec = { "/lib64/ld-linux-x86-64.so.2", "${destination}/${name}" },
    },
  },
  repo = "kivikakk/comrak",
  url = "https://github.com/${repo}/releases/download/${version}/comrak-${short_version}-${arch}",
  version = "v${short_version}",
  short_version = "0.41.0",
}
