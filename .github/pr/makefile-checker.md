# build: add makefile validation test

Tests the actual repo Makefile using GNU make's built-in validation features.

## Changes

- `lib/build/cook.mk` - rules to generate make outputs at build time
- `lib/build/test_makefile.tl` - test reads pre-generated outputs

## Approach

Outputs are generated at build time, not test runtime:
1. `cook.mk` runs `make -n files` and `make -p -n -q` on repo Makefile
2. Output + exit code captured to `o/lib/build/make/*.out`
3. Test reads these files via `TEST_DIR`

## Checks tested

| Output | Validates |
|--------|-----------|
| `dry-run.out` | `make -n files` succeeds (syntax, prerequisites) |
| `database.out` | `make -p` contains expected variables (modules, platform, etc.) |

## Validation

- [x] `make o/lib/build/test_makefile.tl.test.ok` passes
