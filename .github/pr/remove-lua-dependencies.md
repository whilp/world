# 3p: remove luacheck, luaunit, argparse, and lfs dependencies

Remove unused Lua dependencies that are no longer needed.

## Changes

- `3p/luacheck/` - removed luacheck linter infrastructure
- `3p/luaunit/` - removed luaunit testing framework
- `3p/argparse/` - removed (only used by luacheck)
- `3p/lfs/` - removed (referenced non-existent lfs_stub.lua)
- `lib/cosmic/lfs.tl` - removed LFS compatibility stub
- `lib/types/luaunit.d.tl` - removed type definitions

## Build updates

- Makefile: removed luacheck target and vpath references
- lib/cosmic/cook.mk: removed luaunit, argparse, lfs from cosmic bundle
- lib/home/cook.mk: removed luacheck from bundled tools
- 3p/tl/cook.mk: removed argparse dependency
- 3p/lua/cook.mk: removed luaunit dependency

## Test updates

Updated test files to remove references to removed modules:
- lib/cosmic/test_binary.tl
- lib/home/test_versioned.tl
- 3p/lua/test.lua
- 3p/lua/test_release.tl

## Validation

- [x] `make build` succeeds
- [x] cosmic binary builds without luaunit/argparse/lfs
- [x] home binary builds without luacheck
