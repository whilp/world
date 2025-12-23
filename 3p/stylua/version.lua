return {
  version = "2.0.1",
  format = "zip",
  strip_components = 0,
  url = "https://github.com/JohnnyMorganz/StyLua/releases/download/v${version}/stylua-${os}-${arch}.zip",
  platforms = {
    ["darwin-arm64"] = {
      os = "macos",
      arch = "aarch64",
      sha = "3d9caaa660da4b3bc092e805d09af59e42b7504f1253c863b682ea3fc80944f2",
    },
    ["linux-arm64"] = {
      os = "linux",
      arch = "aarch64",
      sha = "3db53cd00a685d0b59f4a4ab188bfa6acb804dca489d810a852ed2ea32eb2b1c",
    },
    ["linux-x86_64"] = {
      os = "linux",
      arch = "x86_64",
      sha = "9087e42f599855192cf4f6a7fb0cb7353e23debd7c749c6e3a76fc58abde3c89",
    },
  },
}
