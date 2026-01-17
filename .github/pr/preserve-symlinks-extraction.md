# home: preserve symlinks with per-tool zips

Refactor the home binary to use per-tool zip files that preserve symlinks.

## Changes

- `Makefile` - add `_zip` target for versioned modules; creates `.local/share/<tool>/<version>/` with symlinks
- `lib/home/cook.mk` - bundle per-tool zips instead of raw directories; simplified build
- `lib/home/main.tl` - extract tool zips at runtime with unzip (preserves symlinks); remove `cmd_list` and `format_mode`
- `lib/home/test_main.tl` - update for new manifest format (`tools` instead of `files`); remove list tests
- `lib/home/test_versioned.tl` - simplify to check tool zips exist and symlinks work after unpack

## How it works

**Build time:**
1. Each 3p module gets a `.zip` target that creates `o/<tool>/.zip`
2. The zip contains `.local/share/<tool>/<version-sha>/` + symlinks at `.local/share/<tool>/`
3. Home binary bundles: `dotfiles.zip`, `tools/*.zip`, `unzip`, `manifest.lua`

**Runtime:**
1. Extract bundled unzip to temp
2. Extract `dotfiles.zip` to destination
3. Extract each `tools/*.zip` to destination (symlinks preserved by unzip)

## Structure

The home binary now contains:
- `/zip/dotfiles.zip` - user config files with symlinks
- `/zip/tools/*.zip` - per-tool zips (gh.zip, nvim.zip, etc.)
- `/zip/unzip` - extraction tool
- `/zip/manifest.lua` - `{ version, tools = {...} }`

## Validation

- [x] `make test` passes (40 tests)
- [x] Symlinks preserved in tool zips (verified with `unzip -Z`)
- [x] Symlinks extracted correctly at runtime
