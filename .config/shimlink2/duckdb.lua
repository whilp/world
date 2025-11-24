return {
  name = "duckdb",
  path = "${name}",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "4dda25dff89b9757dd248f3a48c4d3e215dff64c4c9535a7822b3b7a7f4031c2",
      arch = "osx-arm64",
      ext = "zip",
    },
    ["linux-arm64"] = {
      sha256 = "2b62c2fa4cb2f2e76e937b3b4baf20259cf6a5370e07ff310008ca9d5d6009c4",
      arch = "linux-arm64",
      ext = "zip",
    },
    ["linux-x86_64"] = {
      sha256 = "fae3ba93eedf20b08bca4b23aeac1ba94c446f1c10d029c193e2fc4b4e0bc1bc",
      arch = "linux-amd64",
      ext = "zip",
    },
  },
  repo = "duckdb/duckdb",
  url = "https://github.com/${repo}/releases/download/${version}/duckdb_cli-${arch}.${ext}",
  version = "v1.4.2",
}
