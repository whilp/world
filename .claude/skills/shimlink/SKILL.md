---
name: shimlink
description: Install and update development tool binaries (nvim, luajit, tree-sitter, etc.) using shimlink. Use when updating binary versions, fixing checksums, modifying shimlink config files, debugging luajit bootstrap, or working with shimlink GitHub workflows.
allowed-tools: [Read, Write, Edit, Bash, Glob, Grep]
---

# shimlink

Symlink-based binary manager for installing and managing development tools.

## Architecture

shimlink uses a content-addressed storage model where binaries are stored by their SHA-256 hash:

- config files: `~/.config/shimlink/<binary>.lua` - Lua tables with version, platforms, URLs
- storage: `~/.local/share/shimlink/_/<binary>/<sha-prefix>/` - versioned binaries indexed by first 16 chars of SHA
- bin directory: `~/.local/share/shimlink/bin/` - symlinks pointing to specific versions

## Commands

### show: preview config changes

```bash
shimlink show <binary>
shimlink show <binary> key=value...
```

Preview configuration with optional parameter updates without persisting to disk. Expands templates and generates URLs.

Examples:
```bash
shimlink show nvim
shimlink show nvim version=2025.11.24
shimlink show nvim platform=darwin-arm64 version=2025.11.24 sha=abc123...
```

### write: update config

```bash
shimlink write <binary> key=value...
```

Update configuration and persist to `~/.config/shimlink/<binary>.lua`.

Special parameters:
- `version=X` - updates version (regenerates all platform URLs)
- `platform=Y sha=Z` - updates sha256 for specific platform (requires both)
- other keys use dot notation: `platforms.darwin-arm64.sha256=abc123`

Examples:
```bash
shimlink write nvim version=2025.11.24
shimlink write nvim platform=darwin-arm64 sha=abc123def456...
shimlink write nvim platform=darwin-arm64 version=2025.11.24 sha=abc123def456...
```

Typical workflow when updating a binary:
1. `shimlink show <binary> version=<new>` - preview URLs with new version
2. Download archives for each platform and compute checksums:
   `curl -sL <url> | shasum -a 256`
3. `shimlink write <binary> version=<new> platform=<plat1> sha=<sha1>`
4. Repeat step 3 for each platform
5. `shimlink update <binary>` - install the new version

### update: install binaries

```bash
shimlink update <binary>...
shimlink update -f <binary>
```

Download and install binaries to versioned directories. Creates symlinks in `~/.local/share/shimlink/bin/` pointing to the installed version.

Options:
- `-f, --force` - skip checksum validation and update config with actual checksum

Examples:
```bash
shimlink update nvim
shimlink update -f nvim
shimlink update nvim luajit tree-sitter
```

## Config file structure

Config files in `~/.config/shimlink/` are Lua tables:

```lua
return {
  name = "nvim",
  version = "2025.11.23",
  repo = "whilp/dotfiles",
  path = "bin/${name}",
  strip_components = 1,
  url = "https://github.com/${repo}/releases/download/${version}/${name}-${version}-${arch}.tar.gz",
  platforms = {
    ["darwin-arm64"] = {
      arch = "darwin-arm64",
      sha256 = "877b95fe0d84aaaff51eab66c8c03c2bfc5202c572de6d5b10670159ab83cd2f"
    },
    ["linux-arm64"] = {
      arch = "linux-arm64",
      sha256 = "04f38df6f95c702eb9368d9b64fc04ff74f8027a61ca00181e0100e66fcb75b5"
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "52ee34f0b4cf95c300716b21638f3c8aa4ec6a5761a1d77b44c5f3f4d73701f6"
    }
  }
}
```

Template variables like `${version}`, `${name}`, `${arch}` are interpolated from config values and platform data.

## Checksums

- shimlink checksums are calculated on the **downloaded archive**, not the extracted binary
- the sha256 should be of the archive file itself (.tar.gz, .zip, etc.)
- to get the correct checksum: `curl -sL <url> | shasum -a 256`
- example: `curl -sL https://github.com/.../nvim-2025.11.23-darwin-arm64.tar.gz | shasum -a 256`

## Luajit bootstrap

The luajit bootstrap script `.config/setup/luajit` is used by GitHub workflows to install luajit before running lua-based build scripts:

- downloads pre-built luajit from github releases
- extracts to `~/.local/bootstrap/`
- copies `bin/`, `lib/`, and `share/` directories
- the `share/` directory contains lua modules including dkjson
- version must match the version in the luajit config file

Key files:
- `.config/setup/luajit` - bootstrap script
- `.config/shimlink/luajit.lua` - shimlink config with luajit version and checksums
- `.github/workflows/*.yml` - workflows that use the bootstrap
