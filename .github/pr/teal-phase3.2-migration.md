# Teal phase 3.2: Cosmic module migration

## Summary

Migrate the cosmic module (5 files) to teal using parallel agents, continuing the incremental teal migration plan.

## Changes

### Migrated files

| File | Types added |
|------|-------------|
| `lib/cosmic/init.tl` | `Env` record, `MainFn` type alias, `cosmic` record |
| `lib/cosmic/walk.tl` | `Stat`, `DirHandle`, `FileInfo` records, `Visitor` type, generic `walk<T>` |
| `lib/cosmic/help.tl` | `ModuleInfo`, `HelpModule` records |
| `lib/cosmic/main.tl` | `Opts` record with union types, `LongOpt` type, `SkillModule` record |
| `lib/cosmic/lfs.tl` | `Attrs` record, `lfs` module record |

### Build system changes

- Changed `tlconfig.lua` gen_target from "5.1" to "5.4"
  - Cosmopolitan lua is based on Lua 5.4 with native bitwise operators
  - Lua 5.1 target generated `bit32.band()` calls which don't exist in cosmo-lua
- Updated `lib/cosmic/cook.mk` to reference compiled `main.lua` output

### Documentation

- Updated `docs/teal-migration.md`:
  - Bumped file count from 9 to 14 migrated .tl files
  - Marked PR 3.2 and Batch 3.2 as complete
  - Added detailed notes on types added per file

## Test plan

- [x] `make teal` passes (all 14 .tl files type-check)
- [x] `make clean test` passes (37 tests, 1 skipped)
- [x] Cosmic binary builds and runs correctly
