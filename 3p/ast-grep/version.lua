return {
  version = "0.40.5",
  format = "zip",
  strip_components = 0,
  install = {
    ["ast-grep"] = "bin/ast-grep",
  },
  remove = { "sg" },
  links = {
    ["bin/sg"] = "ast-grep",
  },
  url = "https://github.com/ast-grep/ast-grep/releases/download/{version}/app-{arch}.zip",
  platforms = {
    ["darwin-arm64"] = {
      arch = "aarch64-apple-darwin",
      sha = "55c3a471a483daab49b6413972c3655087f2ce47335ab019067ed1fbf6672107",
    },
    ["linux-arm64"] = {
      arch = "aarch64-unknown-linux-gnu",
      sha = "9596c2abfdf450203e5653e185d805133d5499f8c2cbb00b1aab54754ca70e13",
    },
    ["linux-x86_64"] = {
      arch = "x86_64-unknown-linux-gnu",
      sha = "9715cb5933a4d7fe9e4d8c2be870a9a82840c3f2ec4a57bdff7f15d0912cc676",
    },
  },
}
