# build: fix luacheck requiring 21 staged dependencies

Removes `home_libs` from `home_files` to prevent source file copies from inheriting heavy dependencies meant for the home binary.

- `lib/home/cook.mk` - exclude home_libs from home_files with explanatory comment

## Problem

When running `make luacheck`, all 21 3p tools were being fetched and staged, even though luacheck only needs 5 dependencies (argparse, tl, luacheck, cosmos, luaunit).

The cause: `home_files` included `$(home_libs)` (source file copies), and the Makefile's dependency expansion at lines 105-109 makes `_files` inherit all `_staged` dependencies from `_deps`. Since `home_deps` includes 16+ tools, every file in `home_files` inherited those dependencies.

## Solution

Remove `home_libs` from `home_files`. The libs are already explicit prerequisites of `home_bin`, so they're still built when needed. They just don't need to inherit the heavy dependencies for simple copy operations during linting.

## Validation

- [x] `make luacheck` now fetches only 5 dependencies (down from 21)
- [x] `make home` still builds correctly with all tools
- [x] `make test` passes
