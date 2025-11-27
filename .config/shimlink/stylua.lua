Version{
  name = "stylua",
  path = "${name}",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "3d9caaa660da4b3bc092e805d09af59e42b7504f1253c863b682ea3fc80944f2",
      arch = "macos-aarch64",
      ext = "zip",
    },
    ["linux-arm64"] = {
      sha256 = "3db53cd00a685d0b59f4a4ab188bfa6acb804dca489d810a852ed2ea32eb2b1c",
      arch = "linux-aarch64",
      ext = "zip",
    },
    ["linux-x86_64"] = {
      sha256 = "9087e42f599855192cf4f6a7fb0cb7353e23debd7c749c6e3a76fc58abde3c89",
      arch = "linux-x86_64",
      ext = "zip",
    },
  },
  repo = "JohnnyMorganz/StyLua",
  url = "https://github.com/${repo}/releases/download/${version}/${name}-${arch}.${ext}",
  version = "v${short_version}",
  short_version = "2.0.1",
}
