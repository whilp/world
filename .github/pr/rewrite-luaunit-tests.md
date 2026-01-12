# tests: rewrite luaunit tests to simple assert style

Migrates all test files from luaunit framework to simple assert style for consistency with the test runner.

## Changes

- Delete 11 old `.lua` test files superseded by `.tl` versions
- Rename 7 `test.tl` files to follow `test_*` naming convention
- Convert 18 test files from luaunit to simple assert style
- Update 6 `cook.mk` files to reference new test filenames

## Conversion patterns

- `lu.assertEquals(a, b)` → `assert(a == b, "message")`
- `lu.assertTrue(x)` → `assert(x, "message")`
- `lu.assertNil(x)` → `assert(x == nil, "message")`
- `lu.assertStrContains(s, p)` → `assert(s:find(p, 1, true), "message")`
- `lu.skip(msg)` → `error("SKIP " .. msg)`
- `TestClass:method()` → standalone functions called immediately

## Validation

- [x] All converted tests pass (or skip appropriately)
- [x] Only pre-existing nvim test failure (missing whereami module)
