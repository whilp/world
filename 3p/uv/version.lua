return {
  version = "0.5.7",
  format = "tar.gz",
  strip_components = 1,
  platforms = {
    ["darwin-arm64"] = {
      url = "https://github.com/astral-sh/uv/releases/download/${version}/uv-aarch64-apple-darwin.tar.gz",
      sha = "b8cab25ab2ec0714dbb34179f948c27aa4ab307be54e0628e9e1eef1d2264f9f",
    },
    ["linux-arm64"] = {
      url = "https://github.com/astral-sh/uv/releases/download/${version}/uv-aarch64-unknown-linux-gnu.tar.gz",
      sha = "d4dd7a72689888c92b5191902fd4ec9d25b7eeba07be41ba4a8f89acbb403e2d",
    },
    ["linux-x86_64"] = {
      url = "https://github.com/astral-sh/uv/releases/download/${version}/uv-x86_64-unknown-linux-gnu.tar.gz",
      sha = "8a0a3e823684dec6e49ae17f31bf6483c778fd579671992d9156875210e5161e",
    },
  },
}
