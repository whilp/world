return {
  version = "2.83.2",
  strip_components = 1,
  url = "https://github.com/cli/cli/releases/download/v{version}/gh_{version}_{os}_{arch}.{ext}",
  platforms = {
    ["darwin-arm64"] = {
      os = "macOS",
      arch = "arm64",
      ext = "zip",
      format = "zip",
      sha = "ba3e0396ebbc8da17256144ddda503e4e79c8b502166335569f8390d6b75fa8d",
    },
    ["linux-arm64"] = {
      os = "linux",
      arch = "arm64",
      ext = "tar.gz",
      format = "tar.gz",
      sha = "b1a0c0a0fcf18524e36996caddc92a062355ed014defc836203fe20fba75a38e",
    },
    ["linux-x86_64"] = {
      os = "linux",
      arch = "amd64",
      ext = "tar.gz",
      format = "tar.gz",
      sha = "ca6e7641214fbd0e21429cec4b64a7ba626fd946d8f9d6d191467545b092015e",
    },
  },
}
