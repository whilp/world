return {
  name = "marksman",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "7e18803966231a33ee107d0d26f69b41f2f0dc1332c52dd9729c2e29fb77be83",
      arch = "macos",
    },
    ["linux-arm64"] = {
      sha256 = "b8d6972a56f3f9b7bbbf7c77ef8998e3b66fa82fb03c01398e224144486c9e73",
      arch = "linux-arm64",
    },
    ["linux-x86_64"] = {
      sha256 = "b9cb666c643dfd9b699811fdfc445ed4c56be65c1d878c21d46847f0d7b0e475",
      arch = "linux-x64",
    },
  },
  repo = "artempyanykh/marksman",
  url = "https://github.com/${repo}/releases/download/${version}/${name}-${arch}",
  version = "2024-12-18",
}
