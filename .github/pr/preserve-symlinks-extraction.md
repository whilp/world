# home: preserve symlinks in extraction

Refactor the home binary build and extraction to preserve symlinks (like `lua -> cosmic-lua`).

## Changes

- `lib/home/cook.mk` - restructure build to bundle dotfiles.zip separately from 3p tools
- `lib/home/main.tl` - use unzip at runtime to extract dotfiles (preserves symlinks)
- `lib/home/test_main.tl` - update tests to explicitly use test mode with zip_root

## How it works

**Build time:**
1. Create `dotfiles.zip` containing git-tracked files, compiled nvim configs, cosmic-lua binary, and lua symlink
2. Bundle 3p tools separately under `home/.local/share/`
3. Generate manifest from 3p tools only
4. Bundle `dotfiles.zip` + 3p tools + unzip binary into home binary

**Runtime:**
1. Extract bundled unzip to temp location
2. Use unzip to extract `dotfiles.zip` to destination (preserves symlinks)
3. Copy 3p tools from `/zip/home/` using copy_file
4. Create symlinks for versioned tools in `.local/share/`

## Structure

The home binary now contains:
- `/zip/dotfiles.zip` - user config files with symlinks
- `/zip/home/.local/bin/unzip` - extraction tool
- `/zip/home/.local/share/*/` - 3p tools
- `/zip/manifest.lua` - manifest for 3p tools

## Validation

- [x] `make test` passes (42 tests)
- [x] `make check` passes (298 checks)
- [x] lua symlink preserved in dotfiles.zip
- [x] Symlink extraction verified with unzip -Z
