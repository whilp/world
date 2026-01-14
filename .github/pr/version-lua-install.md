# build: add install/remove/links tables to version.lua

Move post-extraction file transforms from cook.mk to version.lua, keeping module configuration in a single place.

## Changes

- `lib/build/build-stage.tl` - add `apply_install`, `apply_remove`, `apply_links` functions
- `3p/{ruff,delta,duckdb}/version.lua` - add `install` table to move binary to bin/
- `3p/ast-grep/version.lua` - add install + remove + links for sg symlink
- `3p/{ruff,delta,duckdb,ast-grep}/cook.mk` - remove duplicate staging override rules

## New version.lua fields

```lua
install = {
  ["src"] = "dst",  -- move/rename files
},
remove = { "file" },  -- delete unwanted files
links = {
  ["path"] = "target",  -- create symlinks
},
```

## Validation

- [x] `make clean test` passes
- [x] binaries staged correctly in bin/ subdirectory
- [x] ast-grep sg symlink created correctly
