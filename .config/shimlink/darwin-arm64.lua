return {
  aliases = {
    ["nvim-1"] = "nvim",
  },
  binaries = {
    ["ast-grep"] = {
      path = "ast-grep",
      sha256 = "0377bd0d8ea17dcf2e5e76921c7a8324efd4a2b1ae1a321216f896f367da59db",
      url = "https://github.com/ast-grep/ast-grep/releases/download/0.28.0/app-aarch64-apple-darwin.zip",
    },
    biome = {
      sha256 = "c68f2cbe09e9485426a749353a155b1d22c130c6ccdadc7772d603eb247b9a9d",
      url = "https://github.com/biomejs/biome/releases/download/cli%2Fv1.9.4/biome-darwin-arm64",
    },
    comrak = {
      sha256 = "ebff398559a48112e7699ad8ce8a35e1f5f0cf469ed44d55318b1d794abf1090",
      url = "https://github.com/kivikakk/comrak/releases/download/v0.41.0/comrak-0.41.0-aarch64-apple-darwin",
    },
    delta = {
      path = "delta-0.18.2-aarch64-apple-darwin/delta",
      sha256 = "afa92e826b45d93232946e8d5371b7056ca633aef92b5baa6ce08875c274843e",
      url = "https://github.com/dandavison/delta/releases/download/0.18.2/delta-0.18.2-aarch64-apple-darwin.tar.gz",
    },
    gh = {
      path = "gh_2.79.0_macOS_arm64/bin/gh",
      sha256 = "20f0520e9cc872b543bd5e31984f64932b197732c3d3df2c7367d0dc66e7a450",
      url = "https://github.com/cli/cli/releases/download/v2.79.0/gh_2.79.0_macOS_arm64.zip",
    },
    luajit = {
      path = "luajit-2025.10.16-25a61a18-darwin-arm64/bin/luajit",
      sha256 = "2c081318a9a9de0e61d6d8cad6b150043fac550a4fc17ec3cfb8a52f9b5d263c",
      symlinks = {
        ["luajit-2025.10.16-25a61a18-darwin-arm64/share/luajit-2.1"] = "~/.local/share/luajit-2.1",
      },
      url = "https://github.com/whilp/dotfiles/releases/download/luajit-3/luajit-2025.10.16-25a61a18-darwin-arm64.tar.gz",
    },
    marksman = {
      sha256 = "7e18803966231a33ee107d0d26f69b41f2f0dc1332c52dd9729c2e29fb77be83",
      url = "https://github.com/artempyanykh/marksman/releases/download/2024-12-18/marksman-macos",
    },
    nvim = {
      path = "nvim-macos-arm64/bin/nvim",
      sha256 = "46d9ceb410bad2ca1d93bad133360867af7162505040e7efb4fac9fb860a90f4",
      upstream_url = "https://github.com/neovim/neovim/releases/download/nightly/nvim-macos-arm64.tar.gz",
      url = "https://github.com/whilp/dotfiles/releases/download/2025.09.07/nvim-macos-arm64.tar.gz",
    },
    rg = {
      path = "ripgrep-14.1.1-aarch64-apple-darwin/rg",
      sha256 = "0e0cb83f5195f1f51bb8feef1fff5b0b171e82bd1db6bd35deee701a3e7102f8",
      url = "https://github.com/BurntSushi/ripgrep/releases/download/14.1.1/ripgrep-14.1.1-aarch64-apple-darwin.tar.gz",
    },
    ruff = {
      path = "ruff-aarch64-apple-darwin/ruff",
      sha256 = "4122d2367d4580ee316fe20c31004f153bfc591f2c19e5ad25dbc72957ac02c8",
      url = "https://github.com/astral-sh/ruff/releases/download/0.8.4/ruff-aarch64-apple-darwin.tar.gz",
    },
    shfmt = {
      sha256 = "86030533a823c0a7cd92dee0f74094e5b901c3277b43def6337d5e19e56fe553",
      url = "https://github.com/mvdan/sh/releases/download/v3.10.0/shfmt_v3.10.0_darwin_arm64",
    },
    sqruff = {
      path = "sqruff",
      sha256 = "a6267afec08516a040792d7f137509f5e109d53f509b1b5706cd1399151164bb",
      url = "https://github.com/quarylabs/sqruff/releases/download/v0.21.2/sqruff-darwin-aarch64.tar.gz",
    },
    stylua = {
      path = "stylua",
      sha256 = "31b48975b0897317a817471f05a02c5c0b468d3147e10f8cd5cc15c21359c8d7",
      url = "https://github.com/JohnnyMorganz/StyLua/releases/download/v2.0.1/stylua-macos-aarch64.zip",
    },
    superhtml = {
      path = "aarch64-macos/superhtml",
      sha256 = "e20c63e0046fb03bced8a5168267fab2ef71381e9e96c9310f5ea0b7ce3a7ab6",
      url = "https://github.com/kristoff-it/superhtml/releases/download/v0.5.3/aarch64-macos.tar.gz",
    },
    ["tree-sitter"] = {
      path = "tree-sitter",
      sha256 = "15ed2301ef7506ddc0ac844793208eb13dd017f6ae4075cc162e6c4bd9337e28",
      url = "https://github.com/tree-sitter/tree-sitter/releases/download/v0.25.8/tree-sitter-macos-arm64.gz",
    },
    uv = {
      path = "uv-aarch64-apple-darwin/uv",
      sha256 = "de38aa84629780f2568ecdc0d24a62ff4be1e3a12f66b2b7655527a646f06deb",
      url = "https://github.com/astral-sh/uv/releases/download/0.5.7/uv-aarch64-apple-darwin.tar.gz",
    },
  },
}
