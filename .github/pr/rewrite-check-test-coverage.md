# test: rewrite check-test-coverage as proper Teal script

Rewrote the broken `check-test-coverage` shell script as a proper Teal script that leverages Make's module system.

## Added

- `lib/test/check-coverage.tl` - test coverage checker script
- `all_release_tests` - collects `_release_test` and `_release_tests` from modules
- `all_declared_tests` - combines regular and release tests
- `check-test-coverage` target in main Makefile

## Removed

- `lib/test.mk` - dead file, was never included in build

## Changed

- `lib/test/cook.mk` - added check-coverage to build
- `Makefile` - added lib/test to .tl vpath, added check-test-coverage target
- `lib/build/make-help.snap` - updated with new target

## How it works

Make expands module `_tests` variables and passes them as arguments to the script. The script finds all `test_*.tl` files and compares against the declared list. Supports `--test:false` marker to disable specific tests.

## Validation

- [x] `make check-test-coverage` passes
- [x] `make test` passes (42 tests)
- [x] `make check` passes (teal + ast-grep)
