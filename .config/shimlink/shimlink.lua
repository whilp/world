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
      ["darwin-arm64"] = "c9a9e690d94cd9696d2552690fe0abdd2c303e48a3ee5cf9d38728eda054f147",
      ["linux-arm64"] = "62e9e79148be33d27fde24f4dcda83eab207a297ce50fb4a0becfbb29c8f218b",
      ["linux-x86_64"] = "d28be5970afb3e8022210fb9427de0875f1d64f4e4b91ed28b3a3abfebb1d934",
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
  ["darwin-arm64"] = "6ba38dce9f91ee1b9a24aa4aede1db7195258fe176c3f8276ae2d4457d8170a0",
  ["linux-arm64"] = "adf7674086daa4582f598f74ce9caa6b70c1ba8f4a57d2911499b37826b014f9",
  ["linux-x86_64"] = "b7ea845004762358a00ef9127dd9fd723e333c7e4b9cb1da220c3909372310ee",
})

-- duckdb
local duckdb_names = {
  ["darwin-arm64"] = "osx-arm64",
  ["linux-arm64"] = "linux-arm64",
  ["linux-x86_64"] = "linux-amd64",
}
if duckdb_names[platform] then
  binaries.duckdb = {
    path = "duckdb",
    sha256 = ({
      ["darwin-arm64"] = "4dda25dff89b9757dd248f3a48c4d3e215dff64c4c9535a7822b3b7a7f4031c2",
      ["linux-arm64"] = "2b62c2fa4cb2f2e76e937b3b4baf20259cf6a5370e07ff310008ca9d5d6009c4",
      ["linux-x86_64"] = "fae3ba93eedf20b08bca4b23aeac1ba94c446f1c10d029c193e2fc4b4e0bc1bc",
    })[platform],
    url = string.format("https://github.com/duckdb/duckdb/releases/download/v1.4.2/duckdb_cli-%s.zip", duckdb_names[platform]),
  }
end

-- gh (GitHub CLI)
if platform == "darwin-arm64" then
  binaries.gh = archive_binary("cli/cli", "v2.79.0", "gh_2.79.0_macOS_arm64.zip",
    "bin/gh",
    "5454f9509e3dbb8f321310e9e344877d9a01ebb8f8703886b1afb0936d60ffaa",
    1)  -- strip gh_2.79.0_macOS_arm64/
elseif platform == "linux-arm64" then
  binaries.gh = archive_binary("cli/cli", "v2.79.0", "gh_2.79.0_linux_arm64.tar.gz",
    "bin/gh",
    "1b91e546b30181a8ee6d8c72bbf59eaadbb0600bab014dfbcc199676c83ea102",
    1)  -- strip gh_2.79.0_linux_arm64/
elseif platform == "linux-x86_64" then
  binaries.gh = archive_binary("cli/cli", "v2.79.0", "gh_2.79.0_linux_amd64.tar.gz",
    "bin/gh",
    "e7af0c72a607c0528fda1989f7c8e3be85e67d321889002af0e2938ad9c8fb68",
    1)  -- strip gh_2.79.0_linux_amd64/
end

-- luajit
local luajit_version = "2025.10.16-25a61a18"
local luajit_platform_map = {
  ["darwin-arm64"] = "darwin-arm64",
  ["linux-arm64"] = "linux-arm64",
  ["linux-x86_64"] = "linux-x64",
}
local luajit_plat = luajit_platform_map[platform]
if luajit_plat then
  local luajit_sha = ({
    ["darwin-arm64"] = "651395c468e1890f92f3b54ae32a541c8340bcec7343de50227955b6f84bb2f7",
    ["linux-arm64"] = "fe01651e101c9fc237aba229e02ccaf058d13cfe0b8572382aee0ea1bef1c0d0",
    ["linux-x86_64"] = "9109f4c35137d9caf5eee168eeeccaa238d44b86ddf7dbb074c5ccea594d87d8",
  })[platform]
  local luajit_url = string.format("https://github.com/whilp/dotfiles/releases/download/2025.11.23/luajit-%s-%s.tar.gz", luajit_version, luajit_plat)

  binaries.luajit = {
    path = "bin/luajit",
    sha256 = luajit_sha,
    strip_components = 1,
    symlinks = {
      ["share/luajit-2.1"] = "~/.local/share/luajit-2.1",
    },
    url = luajit_url,
  }

  binaries.luarocks = {
    path = "bin/luarocks",
    sha256 = luajit_sha,
    strip_components = 1,
    url = luajit_url,
  }

  binaries["luarocks-admin"] = {
    path = "bin/luarocks-admin",
    sha256 = luajit_sha,
    strip_components = 1,
    url = luajit_url,
  }
end

-- marksman
binaries.marksman = simple_binary("artempyanykh/marksman", "2024-12-18", "marksman", {
  ["darwin-arm64"] = "7e18803966231a33ee107d0d26f69b41f2f0dc1332c52dd9729c2e29fb77be83",
  ["linux-arm64"] = "b8d6972a56f3f9b7bbbf7c77ef8998e3b66fa82fb03c01398e224144486c9e73",
  ["linux-x86_64"] = "b9cb666c643dfd9b699811fdfc445ed4c56be65c1d878c21d46847f0d7b0e475",
}, "", platform_maps.os_simple)

-- nvim
if platform == "darwin-arm64" then
  binaries.nvim = {
    path = "bin/nvim",
    sha256 = "503f3fd7c3926cab7a383c8c2ee4e08ea32a6eaaaf980dc82ea17bbd0f93e593",
    strip_components = 1,
    upstream_url = "https://github.com/neovim/neovim/releases/download/nightly/nvim-macos-arm64.tar.gz",
    url = "https://github.com/whilp/dotfiles/releases/download/2025.11.22/nvim-macos-arm64.tar.gz",
  }
elseif platform == "linux-arm64" then
  binaries.nvim = {
    path = "bin/nvim",
    sha256 = "77d3b791d164937bfd2f63111a426bafc006bd8c72999e569dfbf663c8c962e6",
    strip_components = 1,
    upstream_url = "https://github.com/neovim/neovim/releases/download/nightly/nvim-linux-arm64.tar.gz",
    url = "https://github.com/whilp/dotfiles/releases/download/2025.11.22/nvim-linux-arm64.tar.gz",
  }
elseif platform == "linux-x86_64" then
  binaries.nvim = {
    path = "bin/nvim",
    sha256 = "d50a43f6f0a050e5efe1fb960a741009d6b01b5154778cffa17419551b4b7878",
    strip_components = 1,
    upstream_url = "https://github.com/neovim/neovim/releases/download/nightly/nvim-linux-x86_64.tar.gz",
    url = "https://github.com/whilp/dotfiles/releases/download/2025.11.22/nvim-linux-x86_64.tar.gz",
  }
end

-- rg (ripgrep)
binaries.rg = rust_binary("BurntSushi/ripgrep", "14.1.1", "ripgrep", {
  ["darwin-arm64"] = "24ad76777745fbff131c8fbc466742b011f925bfa4fffa2ded6def23b5b937be",
  ["linux-arm64"] = "c827481c4ff4ea10c9dc7a4022c8de5db34a5737cb74484d62eb94a95841ab2f",
  ["linux-x86_64"] = "4cf9f2741e6c465ffdb7c26f38056a59e2a2544b51f7cc128ef28337eeae4d8e",
}, nil, "rg")

-- ruff
binaries.ruff = rust_binary("astral-sh/ruff", "0.8.4", "ruff", {
  ["darwin-arm64"] = "8893f3ede33a73740f69b10ee9356e5cf2933c0afe146f00176be12ef91bf9d9",
  ["linux-arm64"] = "0dfe36fabb817638863375e0140ce03bf26ccc9a7fd9d2c8e8337b1a21697ed4",
  ["linux-x86_64"] = "c4e6591ae1bb4f15c09c9022b7bfc57e1c3a567acdc9cd76021cd1304b5868c3",
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
      ["darwin-arm64"] = "cb969b42ebbca8229b4484ae2503530c4eef16e23829b340a0b270e1a007e6b6",
      ["linux-arm64"] = "94ef0e55978a960f9cfc717bf5ed2127ae4462cc0a7915d7d38d843e3ca7ddfb",
      ["linux-x86_64"] = "ae09dfcb0d275bf5317769d6eff8aa62c05942369f63ea5e747164a7db9225d9",
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
      ["darwin-arm64"] = "3d9caaa660da4b3bc092e805d09af59e42b7504f1253c863b682ea3fc80944f2",
      ["linux-arm64"] = "3db53cd00a685d0b59f4a4ab188bfa6acb804dca489d810a852ed2ea32eb2b1c",
      ["linux-x86_64"] = "9087e42f599855192cf4f6a7fb0cb7353e23debd7c749c6e3a76fc58abde3c89",
    })[platform],
    url = string.format("https://github.com/JohnnyMorganz/StyLua/releases/download/v2.0.1/stylua-%s.zip", stylua_targets[platform]),
  }
end

-- superhtml
local superhtml_map = platform_maps.arch_os
binaries.superhtml = {
  path = "superhtml",
  sha256 = ({
    ["darwin-arm64"] = "b8b2327f666ff316422061284e107add5c413ebdfdb91774c0c3702a66e65ec9",
    ["linux-arm64"] = "54cd2414de6664b85166a0a2e7c208ca3dbcc935274f4a55309cc9dcfa8e605b",
    ["linux-x86_64"] = "c9fabbbd57851e38a67e6c1eb7942e8bc6189925bfcf437f1e5286932c76d60a",
  })[platform],
  strip_components = 1,
  url = string.format("https://github.com/kristoff-it/superhtml/releases/download/v0.5.3/%s.tar.gz", superhtml_map[platform]),
}

-- tree-sitter
binaries["tree-sitter"] = gz_binary("tree-sitter/tree-sitter", "v0.25.8",
  "tree-sitter-" .. platform_maps.os_arch[platform],
  ({
    ["darwin-arm64"] = "ae3bbba3ba68e759a949e7591a42100a12d660cae165837aba48cae76a599e64",
    ["linux-arm64"] = "cd81d0108df9bdacf4fd32ec53534acced4780540eb5e889c77470d496e37fc5",
    ["linux-x86_64"] = "c9d46697e3e5ae6900a39ad4483667d2ba14c8ffb12c3f863bcf82a9564ee19f",
  })[platform])
if binaries["tree-sitter"] then
  binaries["tree-sitter"].path = "tree-sitter"
end

-- uv
binaries.uv = rust_binary("astral-sh/uv", "0.5.7", "uv", {
  ["darwin-arm64"] = "b8cab25ab2ec0714dbb34179f948c27aa4ab307be54e0628e9e1eef1d2264f9f",
  ["linux-arm64"] = "d4dd7a72689888c92b5191902fd4ec9d25b7eeba07be41ba4a8f89acbb403e2d",
  ["linux-x86_64"] = "8a0a3e823684dec6e49ae17f31bf6483c778fd579671992d9156875210e5161e",
}, platform_maps.rust_triple_gnu, nil, false)

return {
  aliases = {
    ["nvim-1"] = "nvim",
    lua = "luajit",
  },
  binaries = binaries,
}
