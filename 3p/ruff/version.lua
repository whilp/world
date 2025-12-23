return {
  version = "0.8.4",
  format = "tar.gz",
  strip_components = 1,
  platforms = {
    ["darwin-arm64"] = {
      url = "https://github.com/astral-sh/ruff/releases/download/${version}/ruff-aarch64-apple-darwin.tar.gz",
      sha = "8893f3ede33a73740f69b10ee9356e5cf2933c0afe146f00176be12ef91bf9d9",
    },
    ["linux-arm64"] = {
      url = "https://github.com/astral-sh/ruff/releases/download/${version}/ruff-aarch64-unknown-linux-gnu.tar.gz",
      sha = "0dfe36fabb817638863375e0140ce03bf26ccc9a7fd9d2c8e8337b1a21697ed4",
    },
    ["linux-x86_64"] = {
      url = "https://github.com/astral-sh/ruff/releases/download/${version}/ruff-x86_64-unknown-linux-gnu.tar.gz",
      sha = "c4e6591ae1bb4f15c09c9022b7bfc57e1c3a567acdc9cd76021cd1304b5868c3",
    },
  },
}
