return {
  version = "1.4.2",
  format = "zip",
  strip_components = 0,
  url = "https://github.com/duckdb/duckdb/releases/download/v{version}/duckdb_cli-{os}-{arch}.zip",
  platforms = {
    ["darwin-arm64"] = {
      os = "osx",
      arch = "arm64",
      sha = "4dda25dff89b9757dd248f3a48c4d3e215dff64c4c9535a7822b3b7a7f4031c2",
    },
    ["linux-arm64"] = {
      os = "linux",
      arch = "arm64",
      sha = "2b62c2fa4cb2f2e76e937b3b4baf20259cf6a5370e07ff310008ca9d5d6009c4",
    },
    ["linux-x86_64"] = {
      os = "linux",
      arch = "amd64",
      sha = "fae3ba93eedf20b08bca4b23aeac1ba94c446f1c10d029c193e2fc4b4e0bc1bc",
    },
  },
}
