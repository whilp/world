# build: add .zip artifacts for home modules

Modify the build system to create individual .zip artifacts for each 3p module, preserving file types (symlinks) and modes while simplifying extraction.

## Changes

- `lib/build/build-zip.tl` - new script to create versioned .zip artifacts for each staged module
- `lib/build/cook.mk` - add build-zip to build files
- `Makefile` - add .zipped targets and rules to create module zips after staging
- `lib/home/cook.mk` - bundle pre-created .zip artifacts instead of copying directory trees
- `lib/home/main.tl` - extract module zips from `/zip/zips/` during home unpack

## Benefits

- Preserves symlinks and file modes (zip handles this natively)
- Automatic versioned paths like `.local/share/gh/2.83.2-ca6e764/bin/gh`
- Simpler extraction logic (iterate and extract each zip)
- Reduces home build complexity by pre-creating artifacts
