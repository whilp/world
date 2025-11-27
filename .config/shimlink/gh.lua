Version{
  name = "gh",
  path = "bin/${name}",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "5454f9509e3dbb8f321310e9e344877d9a01ebb8f8703886b1afb0936d60ffaa",
      arch = "macOS_arm64",
      ext = "zip",
    },
    ["linux-arm64"] = {
      sha256 = "1b91e546b30181a8ee6d8c72bbf59eaadbb0600bab014dfbcc199676c83ea102",
      arch = "linux_arm64",
      ext = "tar.gz",
    },
    ["linux-x86_64"] = {
      sha256 = "e7af0c72a607c0528fda1989f7c8e3be85e67d321889002af0e2938ad9c8fb68",
      arch = "linux_amd64",
      ext = "tar.gz",
    },
  },
  repo = "cli/cli",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${version}/${name}_${short_version}_${arch}.${ext}",
  version = "v${short_version}",
  short_version = "2.79.0",
}
