return {
  name = "delta",
  path = "${name}",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "6ba38dce9f91ee1b9a24aa4aede1db7195258fe176c3f8276ae2d4457d8170a0",
      arch = "aarch64-apple-darwin",
    },
    ["linux-arm64"] = {
      sha256 = "adf7674086daa4582f598f74ce9caa6b70c1ba8f4a57d2911499b37826b014f9",
      arch = "aarch64-unknown-linux-gnu",
    },
    ["linux-x86_64"] = {
      sha256 = "b7ea845004762358a00ef9127dd9fd723e333c7e4b9cb1da220c3909372310ee",
      arch = "x86_64-unknown-linux-musl",
    },
  },
  repo = "dandavison/delta",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${version}/${name}-${version}-${arch}.tar.gz",
  version = "0.18.2",
}
