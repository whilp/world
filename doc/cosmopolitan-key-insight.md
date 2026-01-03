# the key insight from cosmopolitan BUILD.mk

## what you proposed
```make
o/%: %
	cp $< $@

o/%.luatest.ok: o/% %
	$(lua) $(testrunner) <testfile> <okfile>

o/lib/foo/test_bar.lua: o/some/input.txt
```

## what cosmopolitan does (essentially the same!)

```make
# copy source to build tree
o/$(MODE)/%.o: %.c
	$(CC) -c $< -o $@

# tests depend on built artifacts
o/$(MODE)/%.ok: o/$(MODE)/%
	$< && touch $@

# declare module dependencies
o/$(MODE)/test/foo/%.o: $(FOO_MODULE_DEPS)
```

## the crucial pattern: module-level deps

### cosmopolitan's approach
```make
# declare DIRECT dependencies by module name
TEST_TOOL_NET_DIRECTDEPS = \
	LIBC_SOCK \
	THIRD_PARTY_SQLITE3

# expand to actual files
TEST_TOOL_NET_DEPS := \
	$(call uniq,$(foreach x,$(TEST_TOOL_NET_DIRECTDEPS),$($(x))))

# pattern rule includes module deps
o/$(MODE)/test/tool/net/%.dbg: $(TEST_TOOL_NET_DEPS) o/$(MODE)/test/tool/net/%.o
```

### applied to your proposal
```make
# declare deps by module name (not file paths!)
build_DIRECTDEPS = cosmic
work_DIRECTDEPS = cosmic

# expand to file lists
cosmic_files := $(wildcard lib/cosmic/*.lua)
build_ALL_DEPS := $(cosmic_files)
work_ALL_DEPS := $(cosmic_files)

# pattern rule for o/ copies includes module deps
o/any/lib/build/%.lua: lib/build/%.lua $(build_ALL_DEPS:%=o/any/%)
	@mkdir -p $(@D)
	cp $< $@

# tests depend on o/ copy (which pulls in all deps)
o/luatest/lib/build/%.ok: lib/build/% o/any/lib/build/%
	$(luatest_runner) $< $@
```

## why this is brilliant

### before (manual tracking)
```make
# lib/build/cook.mk - must manually track each dependency
$(luatest_o)/lib/build/test_review.lua.ok: lib/build/review.lua
# oops, forgot that review.lua requires cosmic/spawn.lua!
```

**problems:**
- have to track transitive deps ourselves
- easy to miss dependencies
- duplication across cook.mk files

### after (module-level deps)
```make
# lib/build/cook.mk - declare once per module
build_DIRECTDEPS = cosmic

# Makefile - automatic for all files in module
o/any/lib/build/%.lua: $(build_ALL_DEPS:%=o/any/%)
```

**benefits:**
- declare deps once per module
- make tracks transitive automatically
- systematic, no per-file rules

## the magic of make's dependency tracking

### manual approach (what we'd have to do)
```
test_review depends on review
review depends on spawn
→ we must remember: test_review depends on spawn
```

### make's approach (with o/ tree)
```make
o/luatest/test_review.ok: o/any/review.lua
o/any/review.lua: lib/review.lua o/any/spawn.lua
o/any/spawn.lua: lib/spawn.lua

# make figures out:
# test_review.ok → review.lua → spawn.lua
# therefore: touch spawn.lua → rebuild test_review.ok ✓
```

**we declare direct deps, make handles transitive!**

## answering your original concern

> another problem is that i think we're not specifying transitive dependencies correctly

**solution from cosmopolitan:**
1. don't specify transitive deps explicitly
2. specify DIRECT deps at module level
3. use o/ tree to mirror source with deps
4. let make track the transitive chain

## concrete example

### current problem
```
lib/cosmic/spawn.lua (changed)
  ↓ (required by)
lib/build/review.lua
  ↓ (required by)
lib/build/test_review.lua

# touch lib/cosmic/spawn.lua
# make test_review.lua.ok
# → doesn't rebuild! (missing transitive dep)
```

### with cosmopolitan pattern
```
lib/cosmic/spawn.lua
  ↓ (copy)
o/any/lib/cosmic/spawn.lua
  ↓ (build_ALL_DEPS)
o/any/lib/build/review.lua
  ↓ (copy)
lib/build/review.lua
  ↓ (test depends on o/ copy)
o/luatest/lib/build/test_review.lua.ok

# touch lib/cosmic/spawn.lua
# make test_review.lua.ok
# → rebuilds! make follows the chain ✓
```

## implementation checklist

### phase 1: infrastructure
- [ ] add `o/any/lib/%.lua: lib/%.lua` pattern
- [ ] change test rule to depend on `o/any/lib/%`

### phase 2: module deps (per cook.mk)
- [ ] lib/cosmic/cook.mk: define `cosmic_DIRECTDEPS =`
- [ ] lib/build/cook.mk: define `build_DIRECTDEPS = cosmic`
- [ ] lib/work/cook.mk: define `work_DIRECTDEPS = cosmic`

### phase 3: expand deps (in Makefile or cook.mk)
- [ ] expand DIRECTDEPS to file lists
- [ ] add pattern rules with module deps

### phase 4: verify
- [ ] touch lib/cosmic/spawn.lua
- [ ] verify lib/build tests rebuild
- [ ] verify lib/work tests rebuild

### phase 5: cleanup
- [ ] remove manual .ok deps from cook.mk
- [ ] update documentation

## key takeaway

your intuition was exactly right! the `o/%: %` pattern with explicit deps is:
- **proven** (cosmopolitan uses it)
- **systematic** (module-level deps)
- **automatic** (make handles transitive)
- **standard** (same as C: .c → .o → binary)

cosmopolitan validates this approach at scale (large C codebase with complex deps).
