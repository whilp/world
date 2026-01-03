# test file dependency status

## question
do .ok files depend on their test files? if we touch a test file, will make cause it to re-run?

## answer
**yes** - this works correctly!

## verification

```bash
$ make o/luatest/lib/build/test_review.lua.ok
make: 'o/luatest/lib/build/test_review.lua.ok' is up to date.

$ touch lib/build/test_review.lua

$ make o/luatest/lib/build/test_review.lua.ok
o/any/lua/bin/lua lib/build/luatest.lua lib/build/test_review.lua ...
# test runs!
```

## how it works

### base rule in Makefile
```make
$(luatest_o)/%.ok: % $(luatest_script) $(luaunit) $(script_deps)
	$(TEST_ENV) $(luatest_runner) $< $@ $(TEST_ARGS)
```

the `%` in the prerequisite list matches the test file itself.

### example expansion
```make
# pattern: $(luatest_o)/%.ok: %
# target:  o/luatest/lib/build/test_review.lua.ok
# % matches: lib/build/test_review.lua

# expands to:
o/luatest/lib/build/test_review.lua.ok: lib/build/test_review.lua $(luatest_script) $(luaunit) $(script_deps)
	$(TEST_ENV) $(luatest_runner) lib/build/test_review.lua o/luatest/lib/build/test_review.lua.ok $(TEST_ARGS)
```

## what this means

✓ touching test file → test reruns
✓ test file dependency tracked correctly
✓ no additional rules needed

## remaining dependency issues

while test files themselves are tracked, we're still missing:

1. **same-module dependencies**
   - test uses lib from same module
   - example: test_review.lua uses review.lua
   - solution: see doc/transitive-deps-solution.md

2. **cross-module dependencies**
   - test uses lib from different module
   - example: lib/build tests use cosmic.spawn
   - solution: see doc/transitive-deps-solution.md

3. **test argument files**
   - files passed via TEST_ARGS
   - example: test_luafiles.lua gets manifest files
   - status: **these ARE tracked** (listed in prerequisites)

4. **transitive dependencies**
   - libs that tests import, which import other libs
   - example: review.lua imports cosmic.spawn
   - solution: see doc/transitive-deps-solution.md

## summary

| dependency type | status | notes |
|----------------|--------|-------|
| test → test file | ✓ working | via base rule `%` |
| test → luaunit | ✓ working | via base rule |
| test → luatest_script | ✓ working | via base rule |
| test → script_deps | ✓ working | via base rule |
| test → TEST_ARGS files | ✓ working | explicit in cook.mk |
| test → same module files | ✗ missing | needs wildcard rule |
| test → cross module files | ✗ missing | needs explicit deps |
| test → transitive requires | ✗ missing | needs analysis |

the good news: core test file dependencies work!
the todo: add module-level dependency tracking
