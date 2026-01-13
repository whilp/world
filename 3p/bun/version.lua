return {
  version = "1.3.6",
  format = "zip",
  strip_components = 1,
  url = "https://github.com/oven-sh/bun/releases/download/bun-v{version}/bun-{arch}.zip",
  platforms = {
    ["darwin-arm64"] = {
      arch = "darwin-aarch64",
      sha = "2af1ec8437759ab05b3b0ea421fe9e22e6c705cb4cb0751c326982642dace8fa",
    },
    ["linux-arm64"] = {
      arch = "linux-aarch64",
      sha = "5afd12b366ba2d8297245cc29c039416334dd872152c1db02e5c8aa8c66e96b1",
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha = "9ba98d2134550d6690875b23a4f5c48e74b7cb267e8cc1b8f52605921c6c11ef",
    },
  },
}
