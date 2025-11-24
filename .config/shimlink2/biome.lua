return {
  name = "biome",
  path = "${name}",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "c68f2cbe09e9485426a749353a155b1d22c130c6ccdadc7772d603eb247b9a9d",
      arch = "darwin-arm64",
    },
    ["linux-arm64"] = {
      sha256 = "f0f0f3e7cdec78420a600b05bfc364aa9b804811bd3bbae04e7bf090828ae970",
      arch = "linux-arm64",
    },
    ["linux-x86_64"] = {
      sha256 = "ce247fb644999ef52e5111dd6fd6e471019669fc9c4a44b5699721e39b7032c3",
      arch = "linux-x64",
    },
  },
  repo = "biomejs/biome",
  url = "https://github.com/${repo}/releases/download/${version}/biome-${arch}",
  version = "cli%2Fv1.9.4",
}
