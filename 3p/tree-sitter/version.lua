return {
  version = "0.25.8",
  format = "gz",
  platforms = {
    ["darwin-arm64"] = {
      url = "https://github.com/tree-sitter/tree-sitter/releases/download/v${version}/tree-sitter-macos-arm64.gz",
      sha = "ae3bbba3ba68e759a949e7591a42100a12d660cae165837aba48cae76a599e64",
    },
    ["linux-arm64"] = {
      url = "https://github.com/tree-sitter/tree-sitter/releases/download/v${version}/tree-sitter-linux-arm64.gz",
      sha = "cd81d0108df9bdacf4fd32ec53534acced4780540eb5e889c77470d496e37fc5",
    },
    ["linux-x86_64"] = {
      url = "https://github.com/tree-sitter/tree-sitter/releases/download/v${version}/tree-sitter-linux-x64.gz",
      sha = "c9d46697e3e5ae6900a39ad4483667d2ba14c8ffb12c3f863bcf82a9564ee19f",
    },
  },
}
