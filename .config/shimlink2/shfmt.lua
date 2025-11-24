return {
  name = "shfmt",
  path = "${name}",
  platforms = {
    ["darwin-arm64"] = {
      sha256 = "86030533a823c0a7cd92dee0f74094e5b901c3277b43def6337d5e19e56fe553",
      arch = "darwin_arm64",
    },
    ["linux-arm64"] = {
      sha256 = "9d23013d56640e228732fd2a04a9ede0ab46bc2d764bf22a4a35fb1b14d707a8",
      arch = "linux_arm64",
    },
    ["linux-x86_64"] = {
      sha256 = "1f57a384d59542f8fac5f503da1f3ea44242f46dff969569e80b524d64b71dbc",
      arch = "linux_amd64",
    },
  },
  repo = "mvdan/sh",
  url = "https://github.com/${repo}/releases/download/${version}/${name}_${version}_${arch}",
  version = "v3.10.0",
}
