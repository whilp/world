-- Unified shimlink configuration
-- Platform is passed as the first argument
local platform = ...

-- Platform naming conventions used by different projects
local platform_maps = {
  -- Rust-style target triples
  rust_triple = {
    ["darwin-arm64"] = "aarch64-apple-darwin",
    ["linux-arm64"] = "aarch64-unknown-linux-gnu",
    ["linux-x86_64"] = "x86_64-unknown-linux-musl",
  },
  -- Rust triple but with gnu for both linux variants
  rust_triple_gnu = {
    ["darwin-arm64"] = "aarch64-apple-darwin",
    ["linux-arm64"] = "aarch64-unknown-linux-gnu",
    ["linux-x86_64"] = "x86_64-unknown-linux-gnu",
  },
  -- Simple platform names
  simple = {
    ["darwin-arm64"] = "darwin-arm64",
    ["linux-arm64"] = "linux-arm64",
    ["linux-x86_64"] = "linux-x64",
  },
  -- macOS/Linux naming
  os_simple = {
    ["darwin-arm64"] = "macos",
    ["linux-arm64"] = "linux-arm64",
    ["linux-x86_64"] = "linux-x64",
  },
  -- Arch-OS pattern (superhtml style)
  arch_os = {
    ["darwin-arm64"] = "aarch64-macos",
    ["linux-arm64"] = "aarch64-linux",
    ["linux-x86_64"] = "x86_64-linux-musl",
  },
  -- OS-Arch pattern (tree-sitter style)
  os_arch = {
    ["darwin-arm64"] = "macos-arm64",
    ["linux-arm64"] = "linux-arm64",
    ["linux-x86_64"] = "linux-x64",
  },
}

-- Builder function for Rust-style releases with archives
local function rust_binary(repo, version, name, shas, triple_map, bin_name, include_version)
  triple_map = triple_map or platform_maps.rust_triple
  bin_name = bin_name or name  -- default binary name to package name
  include_version = include_version == nil and true or include_version  -- default to including version
  local triple = triple_map[platform]
  if not triple then return nil end

  local archive_name, dir_name
  if include_version then
    archive_name = string.format("%s-%s-%s", name, version, triple)
    dir_name = archive_name
  else
    archive_name = string.format("%s-%s", name, triple)
    dir_name = archive_name
  end

  return {
    path = bin_name,
    sha256 = shas[platform],
    strip_components = 1,  -- strip the top-level archive directory
    url = string.format("https://github.com/%s/releases/download/%s/%s.tar.gz",
      repo, version, archive_name),
  }
end

-- Builder for simple platform naming
local function simple_binary(repo, version, name, shas, ext, map)
  map = map or platform_maps.simple
  local plat = map[platform]
  if not plat then return nil end

  return {
    sha256 = shas[platform],
    url = string.format("https://github.com/%s/releases/download/%s/%s-%s%s",
      repo, version, name, plat, ext or ""),
  }
end

-- Builder for archive with custom path inside
local function archive_binary(repo, version, archive_name, inner_path, sha256, strip_components)
  return {
    path = inner_path,
    sha256 = sha256,
    strip_components = strip_components or 0,
    url = string.format("https://github.com/%s/releases/download/%s/%s",
      repo, version, archive_name),
  }
end

-- Builder for single-file gzipped binary
local function gz_binary(repo, version, filename, sha256)
  return {
    path = filename,
    sha256 = sha256,
    url = string.format("https://github.com/%s/releases/download/%s/%s.gz",
      repo, version, filename),
  }
end

-- Build the binaries table based on platform
local binaries = {}

-- ast-grep
-- ast-grep releases are named "app-{rust-triple}.zip" and contain ast-grep binary at root
local ast_grep_map = platform_maps.rust_triple_gnu
if ast_grep_map[platform] then
  local triple = ast_grep_map[platform]
  binaries["ast-grep"] = {
    path = "ast-grep",
    sha256 = ({
      ["darwin-arm64"] = "0377bd0d8ea17dcf2e5e76921c7a8324efd4a2b1ae1a321216f896f367da59db",
      ["linux-arm64"] = "147fe3d3857f099df957ff2969eb9adea7b44cceeaa3d50a5e8f4a46acbab134",
      ["linux-x86_64"] = "e34f8222846594570dae0edb99b36a9927b44b86597fb33814c2bf366b583b4a",
    })[platform],
    strip_components = 0,
    url = string.format("https://github.com/ast-grep/ast-grep/releases/download/0.28.0/app-%s.zip", triple),
  }
end

-- biome
binaries.biome = simple_binary("biomejs/biome", "cli%2Fv1.9.4", "biome", {
  ["darwin-arm64"] = "c68f2cbe09e9485426a749353a155b1d22c130c6ccdadc7772d603eb247b9a9d",
  ["linux-arm64"] = "f0f0f3e7cdec78420a600b05bfc364aa9b804811bd3bbae04e7bf090828ae970",
  ["linux-x86_64"] = "ce247fb644999ef52e5111dd6fd6e471019669fc9c4a44b5699721e39b7032c3",
})

-- claude (Linux only)
-- Disabled for now
-- if platform:match("^linux") then
--   local arch = platform:match("arm64") and "arm64" or "x64"
--   binaries.claude = {
--     sha256 = "5f1b6832302ba1ab3e2473e97fb48ea930698d832e86c31cd4e316ec9fb245fb",
--     url = string.format("https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases/1.0.108/linux-%s/claude", arch),
--   }
-- end

-- comrak (single binary, not archived)
local comrak_map = platform_maps.rust_triple
if comrak_map[platform] then
  local comrak_exec
  if platform == "linux-arm64" then
    comrak_exec = { "/lib/ld-linux-aarch64.so.1", "{binary}" }
  elseif platform == "linux-x86_64" then
    comrak_exec = { "/lib64/ld-linux-x86-64.so.2", "{binary}" }
  end

  binaries.comrak = {
    sha256 = ({
      ["darwin-arm64"] = "ebff398559a48112e7699ad8ce8a35e1f5f0cf469ed44d55318b1d794abf1090",
      ["linux-arm64"] = "b76c1a02cd2b2d2b5f9dbde9d16124aa54d9e5a66fa2bc3f5f4d0ce637b1bb64",
      ["linux-x86_64"] = "d3ffc8f04f85a47fa325081affd6b572ad456b542a4d3a1207ef4685afd7e9e2",
    })[platform],
    url = string.format("https://github.com/kivikakk/comrak/releases/download/v0.41.0/comrak-0.41.0-%s", comrak_map[platform]),
    -- comrak is built with nix and has a hardcoded interpreter path, use system linker
    exec = comrak_exec,
  }
end

-- delta
binaries.delta = rust_binary("dandavison/delta", "0.18.2", "delta", {
  ["darwin-arm64"] = "afa92e826b45d93232946e8d5371b7056ca633aef92b5baa6ce08875c274843e",
  ["linux-arm64"] = "7833733f45a128e96757254066b84f6baf553860a656bda4075c32fd735102a0",
  ["linux-x86_64"] = "bb03dd7961db278aa04f0059de8d390ce1f11816e08ae8eebc0867d3aabc9f11",
})

-- gh (GitHub CLI)
if platform == "darwin-arm64" then
  binaries.gh = archive_binary("cli/cli", "v2.79.0", "gh_2.79.0_macOS_arm64.zip",
    "bin/gh",
    "20f0520e9cc872b543bd5e31984f64932b197732c3d3df2c7367d0dc66e7a450",
    1)  -- strip gh_2.79.0_macOS_arm64/
elseif platform == "linux-arm64" then
  binaries.gh = archive_binary("cli/cli", "v2.79.0", "gh_2.79.0_linux_arm64.tar.gz",
    "bin/gh",
    "ee4a9a73720c68152340de6a130ed3bf499d5817a7434bd3d52377acc32a8a67",
    1)  -- strip gh_2.79.0_linux_arm64/
elseif platform == "linux-x86_64" then
  binaries.gh = archive_binary("cli/cli", "v2.79.0", "gh_2.79.0_linux_amd64.tar.gz",
    "bin/gh",
    "da5b8b030353dab06e1f170a40967abece3ee78166723fa66141ba02c3234790",
    1)  -- strip gh_2.79.0_linux_amd64/
end

-- luajit
local luajit_version = "2025.10.16-25a61a18"
if platform == "darwin-arm64" then
  binaries.luajit = {
    path = "bin/luajit",
    sha256 = "09cfbff3ee8c64908d9e1b5d7a2ae734935b519c27e5da371ad1c6eb2fef01bf",
    strip_components = 1,
    symlinks = {
      ["share/luajit-2.1"] = "~/.local/share/luajit-2.1",
    },
    url = string.format("https://github.com/whilp/dotfiles/releases/download/luajit-4/luajit-%s-darwin-arm64.tar.gz", luajit_version),
  }
elseif platform == "linux-arm64" then
  binaries.luajit = {
    path = "bin/luajit",
    sha256 = "d965bdafb98e635767cdf7d827b340c0720c51c31d78198287f1190ce0fc6fa3",
    strip_components = 1,
    symlinks = {
      ["bin/luajit"] = "~/.local/bin/lua-shimlink",
    },
    url = string.format("https://github.com/whilp/dotfiles/releases/download/luajit-4/luajit-%s-linux-arm64.tar.gz", luajit_version),
  }
elseif platform == "linux-x86_64" then
  binaries.luajit = {
    path = "bin/luajit",
    sha256 = "0e21aa2e78b84758e7fe8c1999567b5021ba0839d0d86d42493bd6597e16483f",
    strip_components = 1,
    symlinks = {
      ["bin/luajit"] = "~/.local/bin/lua-shimlink",
    },
    url = string.format("https://github.com/whilp/dotfiles/releases/download/luajit-4/luajit-%s-linux-x64.tar.gz", luajit_version),
  }
end

-- marksman
binaries.marksman = simple_binary("artempyanykh/marksman", "2024-12-18", "marksman", {
  ["darwin-arm64"] = "7e18803966231a33ee107d0d26f69b41f2f0dc1332c52dd9729c2e29fb77be83",
  ["linux-arm64"] = "b8d6972a56f3f9b7bbbf7c77ef8998e3b66fa82fb03c01398e224144486c9e73",
  ["linux-x86_64"] = "b9cb666c643dfd9b699811fdfc445ed4c56be65c1d878c21d46847f0d7b0e475",
}, "", platform_maps.os_simple)

-- nvim
-- NOTE: sha256 checksums are of the extracted bin/nvim binary, not the tar.gz archive
-- To update: curl -sL <url> | tar -xzf - && shasum -a 256 nvim-<platform>/bin/nvim
if platform == "darwin-arm64" then
  binaries.nvim = {
    path = "bin/nvim",
    sha256 = "47d7c27be3a013b306e540c7119132c9d53087dbda29ec182cd1f24230849465",
    strip_components = 1,
    upstream_url = "https://github.com/neovim/neovim/releases/download/nightly/nvim-macos-arm64.tar.gz",
    url = "https://github.com/whilp/dotfiles/releases/download/2025.10.29/nvim-macos-arm64.tar.gz",
  }
elseif platform == "linux-arm64" then
  binaries.nvim = {
    path = "bin/nvim",
    sha256 = "2e40754d63c46ec3251fcadee74c9b33f5a129fb0a189c922b6d80acf0eb9b79",
    strip_components = 1,
    upstream_url = "https://github.com/neovim/neovim/releases/download/nightly/nvim-linux-arm64.tar.gz",
    url = "https://github.com/whilp/dotfiles/releases/download/2025.10.29/nvim-linux-arm64.tar.gz",
  }
elseif platform == "linux-x86_64" then
  binaries.nvim = {
    path = "bin/nvim",
    sha256 = "764a7fb16345507ac281c4e75f8a8e43569073a1bb6a0367b8fed638ca5e55c5",
    strip_components = 1,
    upstream_url = "https://github.com/neovim/neovim/releases/download/nightly/nvim-linux-x86_64.tar.gz",
    url = "https://github.com/whilp/dotfiles/releases/download/2025.10.29/nvim-linux-x86_64.tar.gz",
  }
end

-- rg (ripgrep)
binaries.rg = rust_binary("BurntSushi/ripgrep", "14.1.1", "ripgrep", {
  ["darwin-arm64"] = "0e0cb83f5195f1f51bb8feef1fff5b0b171e82bd1db6bd35deee701a3e7102f8",
  ["linux-arm64"] = "e07d5c85fa9ca740ff4ab8bbac60a1e11c7a5ce242435f7820a03f7c20ef6276",
  ["linux-x86_64"] = "f401154e2393f9002ac77e419f9ee5521c18f4f8cd3e32293972f493ba06fce7",
}, nil, "rg")

-- ruff
binaries.ruff = rust_binary("astral-sh/ruff", "0.8.4", "ruff", {
  ["darwin-arm64"] = "4122d2367d4580ee316fe20c31004f153bfc591f2c19e5ad25dbc72957ac02c8",
  ["linux-arm64"] = "922c84fdb86d1909fe0f691d173f23d875d33b1716d3d8a5cf344e59e1a6c722",
  ["linux-x86_64"] = "ce7b99797145b43d23b732d6050fbbd62e4f9059663cf24218810598753dfb90",
}, platform_maps.rust_triple_gnu, nil, false)

-- shfmt
local shfmt_names = {
  ["darwin-arm64"] = "darwin_arm64",
  ["linux-arm64"] = "linux_arm64",
  ["linux-x86_64"] = "linux_amd64",
}
if shfmt_names[platform] then
  binaries.shfmt = {
    sha256 = ({
      ["darwin-arm64"] = "86030533a823c0a7cd92dee0f74094e5b901c3277b43def6337d5e19e56fe553",
      ["linux-arm64"] = "9d23013d56640e228732fd2a04a9ede0ab46bc2d764bf22a4a35fb1b14d707a8",
      ["linux-x86_64"] = "1f57a384d59542f8fac5f503da1f3ea44242f46dff969569e80b524d64b71dbc",
    })[platform],
    url = string.format("https://github.com/mvdan/sh/releases/download/v3.10.0/shfmt_v3.10.0_%s", shfmt_names[platform]),
  }
end

-- sqruff
local sqruff_targets = {
  ["darwin-arm64"] = "darwin-aarch64",
  ["linux-arm64"] = "linux-aarch64-musl",
  ["linux-x86_64"] = "linux-x86_64-musl",
}
if sqruff_targets[platform] then
  binaries.sqruff = {
    path = "sqruff",
    sha256 = ({
      ["darwin-arm64"] = "a6267afec08516a040792d7f137509f5e109d53f509b1b5706cd1399151164bb",
      ["linux-arm64"] = "fddd62ea4a632544190424ed1fbb8824013314e24dce4eab8d9a5a420341adc5",
      ["linux-x86_64"] = "9ef7797be3e73f72215b95cd6aa2431a62d17daf9d33fe129f2bf2bcc9ecd971",
    })[platform],
    url = string.format("https://github.com/quarylabs/sqruff/releases/download/v0.21.2/sqruff-%s.tar.gz", sqruff_targets[platform]),
  }
end

-- stylua
local stylua_targets = {
  ["darwin-arm64"] = "macos-aarch64",
  ["linux-arm64"] = "linux-aarch64",
  ["linux-x86_64"] = "linux-x86_64",
}
if stylua_targets[platform] then
  binaries.stylua = {
    path = "stylua",
    sha256 = ({
      ["darwin-arm64"] = "31b48975b0897317a817471f05a02c5c0b468d3147e10f8cd5cc15c21359c8d7",
      ["linux-arm64"] = "5848253ab2c10f41c8105c8f94659114078efa592bba18157da816ca50026701",
      ["linux-x86_64"] = "3d2476596a4f7aa7c52fbf67ccd8a6ef208e7e89dfddf9aa2ceb6e195600ad98",
    })[platform],
    url = string.format("https://github.com/JohnnyMorganz/StyLua/releases/download/v2.0.1/stylua-%s.zip", stylua_targets[platform]),
  }
end

-- superhtml
local superhtml_map = platform_maps.arch_os
binaries.superhtml = {
  path = "superhtml",
  sha256 = ({
    ["darwin-arm64"] = "e20c63e0046fb03bced8a5168267fab2ef71381e9e96c9310f5ea0b7ce3a7ab6",
    ["linux-arm64"] = "2450946a457e96a56b3baeac4d54943b3838c723e4beeb3383ff458aa6ac0cf0",
    ["linux-x86_64"] = "aeef7e31e7c1885354be05907252af090b1d6dfce08b51ce09f90a109c3d0cbe",
  })[platform],
  strip_components = 1,
  url = string.format("https://github.com/kristoff-it/superhtml/releases/download/v0.5.3/%s.tar.gz", superhtml_map[platform]),
}

-- tree-sitter
binaries["tree-sitter"] = gz_binary("tree-sitter/tree-sitter", "v0.25.8",
  "tree-sitter-" .. platform_maps.os_arch[platform],
  ({
    ["darwin-arm64"] = "15ed2301ef7506ddc0ac844793208eb13dd017f6ae4075cc162e6c4bd9337e28",
    ["linux-arm64"] = "aa0612920c0149a933e4cebc78e0161e6df47f89f5eb18fd6d4ca7326618b02e",
    ["linux-x86_64"] = "7f78c9df701d7f8155b18c94dd92362bf398ad635426ce323532fe8f20ef0125",
  })[platform])
if binaries["tree-sitter"] then
  binaries["tree-sitter"].path = "tree-sitter"
end

-- uv
binaries.uv = rust_binary("astral-sh/uv", "0.5.7", "uv", {
  ["darwin-arm64"] = "de38aa84629780f2568ecdc0d24a62ff4be1e3a12f66b2b7655527a646f06deb",
  ["linux-arm64"] = "36d8307245f75cc0487c622a07c001f4e0faf224fc0893e6a338faa916b7f3d2",
  ["linux-x86_64"] = "884e9aed698f57e6c56252025342214a9af5993ff8da4580157588726d71fa01",
}, platform_maps.rust_triple_gnu, nil, false)

return {
  aliases = {
    ["nvim-1"] = "nvim",
    lua = "luajit",
  },
  binaries = binaries,
}
