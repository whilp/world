return {
  version = "1.9.4",
  format = "binary",
  platforms = {
    ["darwin-arm64"] = {
      url = "https://github.com/biomejs/biome/releases/download/cli%2Fv${version}/biome-darwin-arm64",
      sha = "c68f2cbe09e9485426a749353a155b1d22c130c6ccdadc7772d603eb247b9a9d",
    },
    ["linux-arm64"] = {
      url = "https://github.com/biomejs/biome/releases/download/cli%2Fv${version}/biome-linux-arm64",
      sha = "f0f0f3e7cdec78420a600b05bfc364aa9b804811bd3bbae04e7bf090828ae970",
    },
    ["linux-x86_64"] = {
      url = "https://github.com/biomejs/biome/releases/download/cli%2Fv${version}/biome-linux-x64",
      sha = "ce247fb644999ef52e5111dd6fd6e471019669fc9c4a44b5699721e39b7032c3",
    },
  },
}
