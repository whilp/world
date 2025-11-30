Version{
  name = "claude",
  platforms = {
    ["darwin-arm64"] = {
      arch = "darwin-arm64",
      sha256 = "08a11782ca5b70c5f0cca4ea4d8b6a54ea0209469e2b561db14c6e222da23b3f"
    },
    ["linux-arm64"] = {
      arch = "linux-arm64-musl",
      sha256 = "6474f28bfaaa28ae32c0a526ca8ba00364cb4fc3eea442f8826221f24c8eccb1"
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "49de548ec2e5e994c4c56b0b47060c4c2bf06fefd7f85889b951518aa84cfa05"
    }
  },
  url = "https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases/${version}/${arch}/${name}",
  version = "2.0.55"
}
