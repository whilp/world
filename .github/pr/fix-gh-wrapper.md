# gh: fix arg passing and simplify binary lookup

The gh/ghc wrappers were broken - `gh --version` silently exited with no output.

Two issues:
1. Used `...` vararg at module level instead of `arg` table for command-line args
2. Version scanning picked up symlinks (`bin`, `share`) that sorted after version strings

Simplified to use the `~/.local/share/gh/bin/gh` symlink directly.

## Changes

- `.local/bin/gh` - fix arg passing, use symlink instead of version scan
- `.local/bin/ghc` - same fixes
