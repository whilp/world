return {
  name = "ruff",
  path = "${name}",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "8893f3ede33a73740f69b10ee9356e5cf2933c0afe146f00176be12ef91bf9d9",
      arch = "aarch64-apple-darwin",
    },
    ["linux-arm64"] = {
      sha256 = "0dfe36fabb817638863375e0140ce03bf26ccc9a7fd9d2c8e8337b1a21697ed4",
      arch = "aarch64-unknown-linux-gnu",
    },
    ["linux-x86_64"] = {
      sha256 = "c4e6591ae1bb4f15c09c9022b7bfc57e1c3a567acdc9cd76021cd1304b5868c3",
      arch = "x86_64-unknown-linux-gnu",
    },
  },
  repo = "astral-sh/ruff",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${version}/${name}-${arch}.tar.gz",
  version = "0.8.4",
}
