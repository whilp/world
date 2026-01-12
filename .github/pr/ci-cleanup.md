# ci: reduce noise and simplify module output paths

Remove spammy output from CI and simplify the build system.

## Changes

- Remove luacheck from CI stages (it doesn't support .tl files)
- Suppress "Wrote:" messages from tl-gen compilation
- Add @ prefix to make recipes to hide mkdir/cp commands
- Simplify module output paths from `o/any/<mod>/lib/<mod>/` to `o/lib/<mod>/`
- Add `_tl_files` mechanism to auto-derive .lua outputs from .tl sources
- Update all lib module cook.mk files to use simpler declarations

## Validation

- [x] `make -j ci` passes
- [x] Incremental builds work correctly
- [x] Test dependencies resolve properly
