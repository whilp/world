return {
  version = "3.10.0",
  format = "binary",
  url = "https://github.com/mvdan/sh/releases/download/v{version}/shfmt_v{version}_{os}_{arch}",
  platforms = {
    ["darwin-arm64"] = {
      os = "darwin",
      arch = "arm64",
      sha = "86030533a823c0a7cd92dee0f74094e5b901c3277b43def6337d5e19e56fe553",
    },
    ["linux-arm64"] = {
      os = "linux",
      arch = "arm64",
      sha = "9d23013d56640e228732fd2a04a9ede0ab46bc2d764bf22a4a35fb1b14d707a8",
    },
    ["linux-x86_64"] = {
      os = "linux",
      arch = "amd64",
      sha = "1f57a384d59542f8fac5f503da1f3ea44242f46dff969569e80b524d64b71dbc",
    },
  },
}
