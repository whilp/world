# build tree approach for test dependencies

## proposed pattern

### copy everything to o/
```make
# general rule: copy source files to o/
o/%.lua: %.lua
	@mkdir -p $(@D)
	cp $< $@
```

### tests depend on o/ copies
```make
# test depends on its o/ copy (which depends on source)
o/%.luatest.ok: o/%.lua
	$(lua) $(testrunner) $< $@
```

### explicit dependencies via o/ files
```make
# test depends on specific files (in o/)
o/lib/foo/test_bar.lua: o/some/input.txt
o/lib/build/test_review.lua: o/lib/build/review.lua
o/lib/build/review.lua: o/lib/cosmic/spawn.lua
```

## how it works

### dependency chain
```
source files          build tree              test results
────────────         ────────────            ──────────────
lib/foo/bar.lua  →   o/lib/foo/bar.lua   →   o/lib/foo/test_bar.luatest.ok
                                              ↑
lib/foo/test_bar.lua → o/lib/foo/test_bar.lua
```

### with explicit deps
```make
# source → build
o/lib/build/review.lua: lib/build/review.lua
o/lib/cosmic/spawn.lua: lib/cosmic/spawn.lua
o/lib/build/test_review.lua: lib/build/test_review.lua

# build deps
o/lib/build/review.lua: o/lib/cosmic/spawn.lua  # review requires spawn

# test deps
o/lib/build/test_review.lua: o/lib/build/review.lua  # test requires review

# test run
o/lib/build/test_review.luatest.ok: o/lib/build/test_review.lua
```

### dependency graph
```
lib/cosmic/spawn.lua ──→ o/lib/cosmic/spawn.lua
                              ↓
lib/build/review.lua ──→ o/lib/build/review.lua
                              ↓
lib/build/test_review.lua ──→ o/lib/build/test_review.lua
                                   ↓
                              o/lib/build/test_review.luatest.ok
```

## benefits

### 1. explicit transitive dependencies
```make
# declare once, make tracks the rest
o/lib/build/review.lua: o/lib/cosmic/spawn.lua

# now ANY file depending on review.lua automatically depends on spawn.lua
o/lib/build/test_review.lua: o/lib/build/review.lua
# test_review transitively depends on spawn.lua ✓
```

### 2. clean separation
- source files: never modified by build
- o/ tree: exact mirror of source with deps resolved
- .ok files: test results

### 3. standard make pattern
- same pattern as C: .c → .o → binary
- easy to understand: o/ is "built artifacts"
- clear what's generated vs source

### 4. easier dependency specification
```make
# current (missing transitive)
$(luatest_o)/lib/build/test_review.lua.ok: lib/build/review.lua

# proposed (transitive via o/ files)
o/lib/build/test_review.lua: o/lib/build/review.lua
```

## challenges

### 1. copying overhead
- every lua file copied to o/
- more disk space
- more I/O

**mitigation:** lua files are small, copy is fast

### 2. LUA_PATH complexity
```make
# needs to point to o/ for requires to work
export LUA_PATH := $(CURDIR)/o/lib/?.lua;$(CURDIR)/o/lib/?/init.lua;...
```

**current state:** we already do this!
```make
export LUA_PATH := $(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;...
```

just change to use o/ instead.

### 3. requires() still need resolution
even with o/ tree, we still need to know:
```lua
local review = require("build.review")  -- which file?
```

**solution:** still need to declare:
```make
o/lib/build/review.lua: REQUIRES = build.review
```

or auto-scan requires.

### 4. bootstrapping
first time, o/ is empty, can't build.

**solution:** pattern rules handle this:
```make
o/%.lua: %.lua
	cp $< $@
```

make automatically builds o/ files as needed.

## comparison with current approach

### current: test from source, manual deps
```make
# Makefile
$(luatest_o)/lib/build/test_review.lua.ok: % $(luatest_script) ...
	$(luatest_runner) $< $@

# lib/build/cook.mk
$(luatest_o)/lib/build/test_review.lua.ok: $(o_any)/build/lib/build/review.lua
```

**pros:**
- simple: test runs from source
- less copying

**cons:**
- transitive deps manual
- cook.mk duplication
- hard to track "what changed"

### proposed: build tree, explicit deps
```make
# Makefile
o/%.lua: %.lua
	cp $< $@

o/%.luatest.ok: o/%.lua
	$(luatest_runner) $< $@

# lib/build/cook.mk
o/lib/build/review.lua: o/lib/cosmic/spawn.lua
o/lib/build/test_review.lua: o/lib/build/review.lua
```

**pros:**
- transitive deps automatic (make handles it)
- explicit, clear deps
- standard build pattern
- easy to see what depends on what

**cons:**
- copying overhead
- more complex directory structure
- requires scanning or manual declaration

## hybrid approach

### copy only what's needed
```make
# don't copy everything, just dependencies
o/lib/build/review.lua: lib/build/review.lua lib/cosmic/spawn.lua
	@mkdir -p $(@D)
	cp lib/build/review.lua $@

# test depends on built version
o/lib/build/test_review.luatest.ok: lib/build/test_review.lua o/lib/build/review.lua
	$(luatest_runner) lib/build/test_review.lua $@
```

**benefit:**
- test runs from source (no copy)
- dependencies explicit via o/ prereqs
- make tracks transitive deps

**drawback:**
- o/ files aren't runnable (they're markers)
- mixed model (some from source, some from o/)

## auto-scanning approach

### scan for requires, generate deps
```make
# scan test file for requires
o/lib/build/test_review.lua.deps: lib/build/test_review.lua
	@mkdir -p $(@D)
	lua extract-requires.lua $< > $@

# include generated deps
-include o/**/*.deps

# deps file contains:
# o/lib/build/test_review.lua: o/lib/build/review.lua
```

### combined with o/ tree
```make
# copy to o/
o/%.lua: %.lua
	@mkdir -p $(@D)
	cp $< $@

# scan for deps
o/%.lua.deps: %.lua
	lua extract-requires.lua $< $@

# include deps
-include $(wildcard o/**/*.deps)

# test
o/%.luatest.ok: o/%.lua
	$(luatest_runner) $< $@
```

**result:**
- automatic dependency discovery
- explicit build tree
- make handles transitive deps
- no manual cook.mk entries

## recommended implementation

### phase 1: o/ tree for lib files (extend current pattern)
```make
# we already do this for some files:
$(o_any)/build/lib/build/%.lua: lib/build/%.lua

# extend to all lib files:
o/any/lib/%.lua: lib/%.lua
	@mkdir -p $(@D)
	cp $< $@
```

### phase 2: explicit deps in cook.mk
```make
# lib/build/cook.mk
o/any/lib/build/review.lua: o/any/lib/cosmic/spawn.lua
o/any/lib/build/test_review.lua: o/any/lib/build/review.lua
```

### phase 3: tests depend on o/ files
```make
# change test rule to depend on o/ copies
$(luatest_o)/lib/%.ok: lib/% o/any/lib/%
	$(luatest_runner) $< $@
```

### phase 4 (later): auto-scan for requires
```make
# generate .deps files from require() scanning
o/any/lib/%.lua.deps: lib/%.lua
	lua scan-requires.lua $< > $@

-include $(wildcard o/any/lib/**/*.deps)
```

## key insight

the o/ tree approach makes transitive dependencies **make's problem**, not ours:

```make
# we declare direct deps:
o/lib/build/review.lua: o/lib/cosmic/spawn.lua
o/lib/build/test_review.lua: o/lib/build/review.lua

# make figures out transitive:
# test_review.lua needs review.lua needs spawn.lua
# so test_review.lua needs spawn.lua ✓
```

this is what make is good at!

## questions

1. should tests run from o/ or from source?
   - o/: requires copying test files
   - source: requires LUA_PATH to point to o/ for deps

2. should we copy all files or just dependencies?
   - all: simple, consistent
   - deps only: less copying, but which deps?

3. how to discover dependencies?
   - manual in cook.mk
   - auto-scan requires()
   - convention (wildcard rules)

## next steps

1. **prototype:** extend o/ copying to all lib files
2. **test:** verify require() works from source with o/ deps
3. **declare:** add explicit deps to cook.mk for known cases
4. **evaluate:** measure complexity vs. benefit
