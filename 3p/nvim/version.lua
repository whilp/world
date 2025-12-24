return {
  version = "0.12.0-dev-1892",
  date = "2025.12.24",
  format = "tar.gz",
  strip_components = 1,
  url = "https://github.com/whilp/dotfiles/releases/download/nvim-nightly-{date}/nvim-{platform}.tar.gz",
  platforms = {
    ["darwin-arm64"] = {
      sha = "665bcc6f7ccc159a12ab33b5ebb457a3051151fdb74fefd5b5c17c12413eb955",
    },
    ["linux-arm64"] = {
      sha = "0202cff00e28eeffeb0d4d2cadf8deabf032bf63b72ed1d3db799cb15e5549fd",
    },
    ["linux-x86_64"] = {
      platform = "linux-x64",
      sha = "13a1f03355366b08a1a8cd48b621e0b93280d51d981b44d4ef7041fd32c2d44d",
    },
  },
}
