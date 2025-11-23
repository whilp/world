---
description: Manage binaries with shimlink
location: user
---

# shimlink

Symlink-based binary manager for installing and managing development tools.

## Configuration

- config: `~/.config/shimlink/shimlink.lua`
- binaries stored in versioned dirs: `~/.local/share/shimlink/_/<binary>/<sha>/`
- shimlink bin directory: `~/.local/share/shimlink/bin/` (symlinks to versioned binaries)

## Commands

```bash
# Update a binary
shimlink update <binary>

# Force update (skip checksum verification)
shimlink update -f <binary>
```

## Checksums

- shimlink checksums are calculated on the **archive/artifact**, not the extracted binary
- the sha256 should be of the downloaded archive file itself
- to get the correct checksum: `curl -sL <url> | shasum -a 256`
- example: `curl -sL https://github.com/.../nvim-macos-arm64.tar.gz | shasum -a 256`

## Examples

```bash
# Update luajit
shimlink update luajit

# Force update without checksum verification
shimlink update -f nvim
```
