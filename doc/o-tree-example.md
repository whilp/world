# concrete example: o/ tree for test_review.lua

## current situation

### manual dependencies
```make
# lib/build/cook.mk
$(luatest_o)/lib/build/test_review.lua.ok: $(o_any)/build/lib/build/review.lua
```

**problem:** doesn't capture that review.lua requires cosmic/spawn.lua

### what we want
touching `lib/cosmic/spawn.lua` should rebuild `test_review.lua.ok`

## proposed solution

### step 1: copy all lib files to o/
```make
# Makefile
o/any/lib/%.lua: lib/%.lua
	@mkdir -p $(@D)
	cp $< $@
```

### step 2: declare direct dependencies
```make
# lib/build/cook.mk - declare what review.lua imports
o/any/lib/build/review.lua: o/any/lib/cosmic/spawn.lua

# lib/build/cook.mk - declare what test imports
o/any/lib/build/test_review.lua: o/any/lib/build/review.lua
```

### step 3: test depends on o/ copy
```make
# Makefile - test depends on its o/ copy
$(luatest_o)/lib/%.ok: lib/% o/any/lib/%
	$(TEST_ENV) $(luatest_runner) $< $@ $(TEST_ARGS)
```

## dependency chain

```
lib/cosmic/spawn.lua
  ↓ (copy)
o/any/lib/cosmic/spawn.lua
  ↓ (declared dep)
o/any/lib/build/review.lua
  ↓ (copy)
lib/build/review.lua
  ↓ (declared dep)
o/any/lib/build/test_review.lua
  ↓ (copy)
lib/build/test_review.lua
  ↓ (test depends on o/ copy)
o/luatest/lib/build/test_review.lua.ok
```

## make's view

```make
# make expands to:
o/luatest/lib/build/test_review.lua.ok: \
    lib/build/test_review.lua \
    o/any/lib/build/test_review.lua

o/any/lib/build/test_review.lua: \
    lib/build/test_review.lua \
    o/any/lib/build/review.lua

o/any/lib/build/review.lua: \
    lib/build/review.lua \
    o/any/lib/cosmic/spawn.lua

o/any/lib/cosmic/spawn.lua: \
    lib/cosmic/spawn.lua
```

**result:** make knows the full dependency graph!

## verification

```bash
# touch the deepest dependency
$ touch lib/cosmic/spawn.lua

# check what needs rebuild
$ make -n o/luatest/lib/build/test_review.lua.ok

# should show:
# 1. cp lib/cosmic/spawn.lua o/any/lib/cosmic/spawn.lua
# 2. cp lib/build/review.lua o/any/lib/build/review.lua
# 3. cp lib/build/test_review.lua o/any/lib/build/test_review.lua
# 4. run test

# all transitive deps rebuilt ✓
```

## what we need to declare

### in Makefile (general rules)
```make
# copy pattern
o/any/lib/%.lua: lib/%.lua
	@mkdir -p $(@D)
	cp $< $@

# test pattern
$(luatest_o)/lib/%.ok: lib/% o/any/lib/%
	$(TEST_ENV) $(luatest_runner) $< $@ $(TEST_ARGS)
```

### in lib/build/cook.mk (specific deps)
```make
# review.lua imports
o/any/lib/build/review.lua: o/any/lib/cosmic/spawn.lua

# manifest.lua imports
o/any/lib/build/manifest.lua: o/any/lib/cosmic/spawn.lua o/any/lib/cosmic/walk.lua

# luacheck.lua imports
o/any/lib/build/luacheck.lua: o/any/lib/cosmic/spawn.lua

# tests import from build module
o/any/lib/build/test_review.lua: o/any/lib/build/review.lua
o/any/lib/build/test_fetch.lua: o/any/lib/build/fetch.lua
o/any/lib/build/test_install.lua: o/any/lib/build/install.lua
```

### in lib/cosmic/cook.mk (specific deps)
```make
# cosmic module has no external deps
# (only builtin cosmo.unix, cosmo.path)
```

## comparison

### lines of code

**current:**
```make
# lib/build/cook.mk - per test
$(luatest_o)/lib/build/test_review.lua.ok: $(o_any)/build/lib/build/review.lua
$(luatest_o)/lib/build/test_fetch.lua.ok: $(o_any)/build/lib/build/fetch.lua
# ... (missing transitive deps)
```

**proposed:**
```make
# Makefile - once (general)
o/any/lib/%.lua: lib/%.lua
	@mkdir -p $(@D); cp $< $@

# lib/build/cook.mk - per module (captures all deps)
o/any/lib/build/review.lua: o/any/lib/cosmic/spawn.lua
o/any/lib/build/manifest.lua: o/any/lib/cosmic/spawn.lua o/any/lib/cosmic/walk.lua
o/any/lib/build/test_review.lua: o/any/lib/build/review.lua
```

### benefits
- fewer lines (generic pattern vs. per-test rules)
- complete (captures transitive deps)
- maintainable (declare imports once)
- standard (same pattern as C .o files)

### trade-offs
- more copying (all lib files to o/)
- more explicit (must declare imports)
- different model (build tree vs. run from source)

## migration path

### phase 1: add copy pattern
```make
o/any/lib/%.lua: lib/%.lua
	@mkdir -p $(@D)
	cp $< $@
```

### phase 2: tests depend on o/ copy
```make
$(luatest_o)/lib/%.ok: lib/% o/any/lib/%
	$(TEST_ENV) $(luatest_runner) $< $@ $(TEST_ARGS)
```

### phase 3: declare direct deps incrementally
```make
# start with one module
o/any/lib/cosmic/spawn.lua: lib/cosmic/spawn.lua  # explicit (redundant with pattern)
o/any/lib/build/review.lua: o/any/lib/cosmic/spawn.lua  # new dep
```

### phase 4: remove old manual deps
```make
# remove from lib/build/cook.mk:
# $(luatest_o)/lib/build/test_review.lua.ok: $(o_any)/build/lib/build/review.lua
```

## key insight

the o/ tree approach shifts the problem from:
- **current:** manually tracking transitive deps in cook.mk
- **proposed:** declaring direct imports, let make track transitive

this is what make was designed for!
