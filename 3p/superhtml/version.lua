return {
  version = "0.5.3",
  format = "tar.gz",
  strip_components = 1,
  url = "https://github.com/kristoff-it/superhtml/releases/download/v{version}/{arch}-{os}.tar.gz",
  platforms = {
    ["darwin-arm64"] = {
      arch = "aarch64",
      os = "macos",
      sha = "b8b2327f666ff316422061284e107add5c413ebdfdb91774c0c3702a66e65ec9",
    },
    ["linux-arm64"] = {
      arch = "aarch64",
      os = "linux",
      sha = "54cd2414de6664b85166a0a2e7c208ca3dbcc935274f4a55309cc9dcfa8e605b",
    },
    ["linux-x86_64"] = {
      arch = "x86_64",
      os = "linux-musl",
      sha = "c9fabbbd57851e38a67e6c1eb7942e8bc6189925bfcf437f1e5286932c76d60a",
    },
  },
}
