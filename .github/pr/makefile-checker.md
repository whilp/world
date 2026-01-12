# checker: add makefile static analysis test

Tests GNU make's built-in checking capabilities for Makefile validation.

## Changes

- `lib/checker/fixtures/make/*.mk` - test Makefile fixtures
- `lib/checker/cook.mk` - rules to generate make outputs at build time
- `lib/checker/test_makefile.tl` - test reads pre-generated outputs

## Approach

Fixtures are checked at build time, not test runtime:
1. `cook.mk` runs `make -n`, `--warn-undefined-variables`, `-p` on each fixture
2. Output + exit code captured to `o/lib/checker/fixtures/make/*.out`
3. Test reads these files via `TEST_DIR`

## Checks tested

| Fixture | Validates |
|---------|-----------|
| `valid.mk` | Clean makefile passes |
| `syntax-error.mk` | `make -n` catches syntax errors |
| `undefined-var.mk` | `--warn-undefined-variables` detects usage |
| `defined-var.mk` | Defined vars don't warn |
| `missing-prereq.mk` | Missing dependencies detected |
| `circular-dep.mk` | Circular dependency warnings |
| `database.mk` | `make -p` dumps variables/rules |

## Validation

- [x] `make o/lib/checker/test_makefile.tl.test.ok` passes
