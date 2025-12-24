return {
  version = "2024-12-18",
  format = "binary",
  url = "https://github.com/artempyanykh/marksman/releases/download/{version}/marksman-{platform}",
  platforms = {
    ["darwin-arm64"] = {
      platform = "macos",
      sha = "7e18803966231a33ee107d0d26f69b41f2f0dc1332c52dd9729c2e29fb77be83",
    },
    ["linux-arm64"] = {
      platform = "linux-arm64",
      sha = "b8d6972a56f3f9b7bbbf7c77ef8998e3b66fa82fb03c01398e224144486c9e73",
    },
    ["linux-x86_64"] = {
      platform = "linux-x64",
      sha = "b9cb666c643dfd9b699811fdfc445ed4c56be65c1d878c21d46847f0d7b0e475",
    },
  },
}
