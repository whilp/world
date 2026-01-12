# lib: remove lib/work and luaposix dependencies

Remove the work module and all code depending on luaposix.

## Removed

- `lib/work/` - work item management module (17 files)
- `lib/file.tl` - file utilities using luaposix (unused)
- `lib/types/posix/` - luaposix type declarations (8 files)
- `.config/nvim/lua/work/` - neovim work plugin (7 files)
- `.config/nvim/plugin/work.tl` - nvim plugin entry point
- `.claude/skills/work/` - work skill
- `docs/teal-migration.md` - migration tracking doc
- `manifest-lua-tests-before.txt` - test manifest

## Updated

- `lib/cook.mk` - removed work include
- `lib/test.mk` - removed work references

## Validation

- [x] `make clean test` passes (42 tests)
