# build: add unveil/pledge constraints for build security

Constrain filesystem access during fetch, stage, and test phases using two complementary approaches:

1. **sandbox.lua** - Lua-level wrapper that applies `unix.unveil()` before running scripts. Works with regular GNU make.

2. **landlock-make** - Makefile-level constraints using `.PLEDGE`/`.UNVEIL` variables. Only effective when using landlock-make binary.

## Changes

- lib/build/sandbox.lua - centralized unveil profiles for fetch/stage/test
- lib/build/cook.mk - wire up sandbox wrapper for build commands
- lib/test/cook.mk - wire up sandbox wrapper for test runner
- lib/build/build-fetch.lua - add SANDBOX_MAIN check
- lib/build/build-stage.lua - add SANDBOX_MAIN check
- lib/test/run-test.lua - delegate unveil/tmpdir to sandbox
- Makefile - add .PLEDGE/.UNVEIL for landlock-make compatibility
- 3p/landlock-make/ - fetch landlock-make binary

## Constraints

| Phase | Pledge | Unveil |
|-------|--------|--------|
| fetch | stdio rpath wpath cpath inet dns | r:version r:/etc/ssl rwc:output |
| stage | stdio rpath wpath cpath proc exec | r:archive rwc:output rx:/usr/bin |
| test | stdio rpath wpath cpath proc exec | r:sources rwc:/tmp rwc:output rx:/usr |

## Usage

```bash
# Regular make (uses sandbox.lua)
make test

# With landlock-make (additional enforcement)
make staged
./o/staged/landlock-make/*/bin/landlock-make test
```

## Validation

- [x] make clean test passes
- [x] incremental builds work
- [x] fetch/stage/test all respect constraints
