return {
  version = "2.79.0",
  strip_components = 1,
  url = "https://github.com/cli/cli/releases/download/v${version}/gh_${version}_${os}_${arch}.${ext}",
  platforms = {
    ["darwin-arm64"] = {
      os = "macOS",
      arch = "arm64",
      ext = "zip",
      format = "zip",
      sha = "5454f9509e3dbb8f321310e9e344877d9a01ebb8f8703886b1afb0936d60ffaa",
    },
    ["linux-arm64"] = {
      os = "linux",
      arch = "arm64",
      ext = "tar.gz",
      format = "tar.gz",
      sha = "1b91e546b30181a8ee6d8c72bbf59eaadbb0600bab014dfbcc199676c83ea102",
    },
    ["linux-x86_64"] = {
      os = "linux",
      arch = "amd64",
      ext = "tar.gz",
      format = "tar.gz",
      sha = "e7af0c72a607c0528fda1989f7c8e3be85e67d321889002af0e2938ad9c8fb68",
    },
  },
}
