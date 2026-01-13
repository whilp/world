# build: fix parallel build race in test-release

Fixed race condition in `make -j test-release` where two targets tried to build `$(home_bin)` simultaneously.

- lib/home/cook.mk - `.tested` now depends on `home-release` instead of `$(home_bin)` directly

## Root cause

When running `make -j test-release`:
1. The `.tested` target had `$(home_bin)` as a dependency, triggering the main make to rebuild it
2. `home-release` called a sub-make to rebuild `$(home_bin)` with `HOME_NVIM_DIR=$(nvim_bundle_out)`

Their recipes interfered - one's `rm -rf $(home_built)` deleted directories while the other was mid-build.

## Validation

- [x] Reproduced the race locally
- [x] Verified fix with `make clean && make -j check test build && make -j test-release`
