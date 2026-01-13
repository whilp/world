return {
  version = "3.1.3",
  format = "binary",
  url = "https://github.com/whilp/world/releases/download/clasp-v{version}/clasp-{version}-{arch}",
  platforms = {
    ["darwin-arm64"] = {
      arch = "darwin-arm64",
      sha = "TODO",  -- run build-release.sh on darwin-arm64, then update
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha = "TODO",  -- run build-release.sh on linux-arm64, then update
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha = "TODO",  -- run build-release.sh on linux-x64, then update
    },
  },
}
