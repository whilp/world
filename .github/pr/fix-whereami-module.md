# build: fix test dependency tracking for lib modules

Fixes race condition in parallel builds where nvim tests would run before whereami module was compiled, causing "module 'whereami' not found" errors.

The test dependency expansion only tracked `_dir` for dependencies, which only exists for versioned (3p) modules. When a lib module like `whereami` was a dependency, its compiled files weren't being tracked as prerequisites.

- Makefile:186-202 - split dependency computation into two passes to ensure all `_tl_lua` variables are computed before setting up test dependencies

## Changes

Split test dependency expansion into:
1. First pass: compute all `_tl_lua` variables for every module
2. Second pass: set up test dependencies using those variables

Now tests depend on both `_dir` (for 3p modules) and `_files`/`_tl_lua` (for lib modules).

## Validation

- [x] `make clean && make -j test only=nvim` passes consistently
- [x] verified `o/lib/whereami/init.lua` now appears in nvim test prerequisites
