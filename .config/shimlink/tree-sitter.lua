Version{
  name = "tree-sitter",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "ae3bbba3ba68e759a949e7591a42100a12d660cae165837aba48cae76a599e64",
      arch = "macos-arm64",
    },
    ["linux-arm64"] = {
      sha256 = "cd81d0108df9bdacf4fd32ec53534acced4780540eb5e889c77470d496e37fc5",
      arch = "linux-arm64",
    },
    ["linux-x86_64"] = {
      sha256 = "c9d46697e3e5ae6900a39ad4483667d2ba14c8ffb12c3f863bcf82a9564ee19f",
      arch = "linux-x64",
    },
  },
  repo = "tree-sitter/tree-sitter",
  url = "https://github.com/${repo}/releases/download/${version}/${name}-${arch}.gz",
  version = "v0.25.8",
}
