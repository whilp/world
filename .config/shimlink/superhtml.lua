Version{
  name = "superhtml",
  path = "${name}",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "b8b2327f666ff316422061284e107add5c413ebdfdb91774c0c3702a66e65ec9",
      arch = "aarch64-macos",
    },
    ["linux-arm64"] = {
      sha256 = "54cd2414de6664b85166a0a2e7c208ca3dbcc935274f4a55309cc9dcfa8e605b",
      arch = "aarch64-linux",
    },
    ["linux-x86_64"] = {
      sha256 = "c9fabbbd57851e38a67e6c1eb7942e8bc6189925bfcf437f1e5286932c76d60a",
      arch = "x86_64-linux-musl",
    },
  },
  repo = "kristoff-it/superhtml",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${version}/${arch}.tar.gz",
  version = "v0.5.3",
}
