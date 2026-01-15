# home/setup/claude: install to bin subdirectory

Fix Claude installation path to match zshenv glob pattern. The `.zshenv` file includes `~/.local/share/*/*-*/bin` in PATH, but Claude was being installed to `<version>-<sha>/claude` instead of `<version>-<sha>/bin/claude`.

## Changes

- `lib/home/setup/claude.tl` - Install Claude binary to `bin` subdirectory to match the pattern used by other versioned tools (nvim, gh, delta)
