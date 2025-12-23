return {
  version = "0.18.2",
  format = "tar.gz",
  strip_components = 1,
  url = "https://github.com/dandavison/delta/releases/download/${version}/delta-${version}-${arch}.tar.gz",
  platforms = {
    ["darwin-arm64"] = {
      arch = "aarch64-apple-darwin",
      sha = "6ba38dce9f91ee1b9a24aa4aede1db7195258fe176c3f8276ae2d4457d8170a0",
    },
    ["linux-arm64"] = {
      arch = "aarch64-unknown-linux-gnu",
      sha = "adf7674086daa4582f598f74ce9caa6b70c1ba8f4a57d2911499b37826b014f9",
    },
    ["linux-x86_64"] = {
      arch = "x86_64-unknown-linux-musl",
      sha = "b7ea845004762358a00ef9127dd9fd723e333c7e4b9cb1da220c3909372310ee",
    },
  },
}
