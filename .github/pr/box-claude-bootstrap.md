# box: add Claude binary installation

Add Claude binary download/install to box bootstrapping so sprite boxes have Claude available.

## Problem

Box bootstrap configures Claude credentials and settings, but doesn't install the Claude binary itself. The binary download logic was in `lib/home/setup/claude.tl` (Codespaces only).

## Solution

New `lib/box/claude.tl` module that downloads and installs Claude binary using version info from `lib/claude/version.lua`. Detects platform dynamically using cosmo.GetHostOs/GetHostIsa.

## Changes

- `lib/box/claude.tl` - new module with `install()` and `detect_platform()`
- `lib/box/run.tl` - call `claude.install()` in bootstrap
- `lib/box/cook.mk` - bundle `lib/claude/version.lua` into box binary
- `lib/home/setup/claude.tl` - delete entirely
