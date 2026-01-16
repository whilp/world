# release: add bootstrap-home executable asset

Add a Lua executable asset to releases that downloads and runs the appropriate home binary for the current platform.

## Changes

- `lib/bootstrap-home/init.tl` - detects platform, fetches matching home asset from latest release, validates SHA256, and execs with `-v -f`
- `lib/bootstrap-home/cook.mk` - module definition for bootstrap-home
- `lib/build/gen-bootstrap-shas.tl` - build script that computes SHA256 for each platform's home binary and generates shas.lua
- `lib/build/cook.mk` - added gen-bootstrap-shas to build_files
- `lib/cook.mk` - includes bootstrap-home module
- `Makefile` - release target builds bootstrap-home by bundling cosmic with bootstrap-home module and generated SHAs

## How it works

At build time:
1. gen-bootstrap-shas computes SHA256 for each home-{platform} artifact
2. Generates o/lib/bootstrap-home/shas.lua with hardcoded SHAs
3. Bundles cosmic executable with bootstrap-home module and shas.lua
4. Result is a single executable that can bootstrap any platform

At runtime:
1. Detects current platform using cosmo.GetHostOs() and cosmo.GetHostIsa()
2. Fetches latest release info from GitHub API
3. Downloads matching home-{platform} binary
4. Validates SHA256 against hardcoded value
5. Writes to temp file and execs with `-v -f`

## Usage

```bash
curl -fsSL https://github.com/whilp/world/releases/latest/download/bootstrap-home | sh -s
```

This provides a universal bootstrap that works across all supported platforms without needing platform detection in the download command.
