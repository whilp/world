# build: use tl.lua as library to avoid argparse dependency

The teal CLI (`tl`) requires `argparse` at runtime, which was removed in
commit a9b02229. This broke CI builds.

Instead of restoring argparse, use `lib/cosmic/tl-gen.lua` which calls
`tl.lua` directly as a library, avoiding the CLI wrapper entirely.

## Changes

- `lib/cook.mk` - use lib/cosmic/tl-gen.lua instead of $(tl_staged)/tl
- `3p/tl/cook.mk` - simplify tl_gen to use lib/cosmic/tl-gen.lua
- `3p/tl/tl-gen.lua` - removed (no longer needed)
- `3p/tl/tl-gen.tl` - removed (no longer needed)
