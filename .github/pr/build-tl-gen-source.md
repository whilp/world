# build: use source tl-gen.lua instead of bundled version

Use `lib/cosmic/tl-gen.lua` directly instead of the bundled `/zip/tl-gen.lua`
to suppress "Wrote:" messages during bootstrap.

## Problem

After `make clean`, the CI output showed 6 spammy "Wrote:" lines:
```
Wrote: build-fetch.lua
Wrote: test-snap.lua
Wrote: make-help.lua
Wrote: reporter.lua
Wrote: build-stage.lua
Wrote: check-update.lua
```

The source file `lib/cosmic/tl-gen.lua` already had this output removed in
commit bc480e1, but the pre-built `bin/cosmic-lua` binary (from release
2026-01-12-5a4658b) still contains the old bundled version with the print.

## Solution

Instead of running the bundled `/zip/tl-gen.lua`, run the source file directly.
Cosmic can execute it since it has `tl.lua` bundled.

## Changes

- lib/build/cook.mk - use source tl-gen.lua, add it as dependency

## Validation

- [x] `make clean && make -j ci` passes with no "Wrote:" output
- [x] `make -j ci` second run is fully cached (8 lines)
