Version{
  name = "uv",
  path = "${name}",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "b8cab25ab2ec0714dbb34179f948c27aa4ab307be54e0628e9e1eef1d2264f9f",
      arch = "aarch64-apple-darwin",
    },
    ["linux-arm64"] = {
      sha256 = "d4dd7a72689888c92b5191902fd4ec9d25b7eeba07be41ba4a8f89acbb403e2d",
      arch = "aarch64-unknown-linux-gnu",
    },
    ["linux-x86_64"] = {
      sha256 = "8a0a3e823684dec6e49ae17f31bf6483c778fd579671992d9156875210e5161e",
      arch = "x86_64-unknown-linux-gnu",
    },
  },
  repo = "astral-sh/uv",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${version}/${name}-${arch}.tar.gz",
  version = "0.5.7",
}
