# checker: add makefile static analysis test

Tests GNU make's built-in checking capabilities for Makefile validation.

## Changes

- `lib/checker/test_makefile.tl` - test suite for make-based static analysis

## Checks tested

| Check | Command | Detects |
|-------|---------|---------|
| Syntax errors | `make -n` | Invalid makefile syntax |
| Undefined variables | `make --warn-undefined-variables` | `$(UNDEFINED_VAR)` usage |
| Missing prerequisites | `make -n` | Dependencies without rules |
| Circular dependencies | `make -n` | `a: b` / `b: a` cycles |
| Database dump | `make -p -n` | All variables and rules |

## Helper function

Includes `check_makefile(dir, target)` that aggregates results:

```lua
local result = check_makefile(dir)
-- result.syntax_ok        -- boolean
-- result.undefined_vars   -- {string}
-- result.missing_prereqs  -- {string}
-- result.has_circular_deps -- boolean
-- result.errors           -- {string}
```

## Validation

- [x] `make o/lib/checker/test_makefile.tl.test.ok` passes
