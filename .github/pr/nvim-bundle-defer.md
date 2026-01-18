# nvim: defer bundling to release time

Defer nvim bundling to release time to speed up local development builds.

**Before:** `make build` compiled treesitter parsers and bundled all nvim plugins.

**After:**
- `make build` uses raw nvim binary (fast)
- `make test-release` builds the full bundle and rebuilds home with it

## Changes

- 3p/nvim/cook.mk - split tests into basic and release
- lib/home/cook.mk - add HOME_NVIM_DIR and home-release target

## Validation

- [ ] `make test` passes (uses raw nvim, no bundle)
- [ ] `make build` completes without building parsers
- [ ] `make test-release` builds bundle and passes
