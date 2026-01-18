return {
  version = "0.0.1-rc30",
  format = "tar.gz",
  strip_components = 1,
  install = {
    ["sprite"] = "bin/sprite",
  },
  url = "https://sprites-binaries.t3.storage.dev/client/v{version}/sprite-{arch}.tar.gz",
  platforms = {
    ["darwin-arm64"] = {
      arch = "darwin-arm64",
      sha = "baf3dafc2452d8755d537e5a2c7779b38523de22f6cd6cbea4c27c04b82946cb",
    },
    ["darwin-x86_64"] = {
      arch = "darwin-amd64",
      sha = "8b62f0afaf426bca3edca81b4a247d2b4210491beef4cdcdafe6d5735cbb575a",
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha = "19a38cef766cfecf45ea714a93568ac5b72a0e175ab80948ccc909325df50d12",
    },
    ["linux-x86_64"] = {
      arch = "linux-amd64",
      sha = "9b6f4192987061133a8fb6aa8a0cc21202dace7799ce814240112aa721118305",
    },
  },
}
