# claude: fix bin symlink for installed binaries

The Claude install puts binaries at `~/.local/share/claude/{version}-{sha}/bin/claude`, but the wrapper's `scan_for_atomic_install()` was looking in the wrong path and the stable fallback path needed a symlink.

Also creates a default `~/.claude.json` config file on first install.

## Changes

- `lib/claude/main.tl` - fix `scan_for_atomic_install()` to look in `bin/` subdirectory
- `lib/box/claude.tl` - create `bin` -> `{version}-{sha}/bin` symlink after install
- `lib/box/claude.tl` - create default `~/.claude.json` with theme and settings
