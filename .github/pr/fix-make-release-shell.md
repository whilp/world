# build: fix make release shell compatibility

The `make release` target was failing with "Bad substitution" error because it uses bash-specific parameter substitution syntax (`${GITHUB_SHA::7}`) but Make was invoking recipes with `/bin/sh`, which on Linux is typically dash and doesn't support this feature.

## Changes

- Makefile:2 - Added `SHELL := /bin/bash` to ensure all recipes use bash

## Validation

- [x] Verified bash substring syntax works correctly
- [x] Confirmed release target uses the problematic syntax
