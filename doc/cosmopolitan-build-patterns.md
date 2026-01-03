# lessons from cosmopolitan's BUILD.mk

## source
https://github.com/jart/cosmopolitan/blob/master/test/tool/net/BUILD.mk

## key patterns

### 1. MODE-based build tree
```make
MODE ?= opt  # or dbg, tiny, etc.
o/$(MODE)/test/tool/net/%.o
o/$(MODE)/test/tool/net/%.ok
```

**benefit:** multiple build modes in parallel
**our equivalent:** could use `o/$(mode)/` but we use `o/$(platform)/`

### 2. wildcard discovery + filtering
```make
TEST_TOOL_NET_FILES := $(wildcard test/tool/net/*)
TEST_TOOL_NET_SRCS = $(filter %.c,$(TEST_TOOL_NET_FILES))
TEST_TOOL_NET_SRCS_TEST = $(filter %_test.c,$(TEST_TOOL_NET_SRCS))
TEST_TOOL_NET_LUAS_TEST = $(filter %_test.lua,$(TEST_TOOL_NET_FILES))
```

**benefit:** automatic file discovery, no manual lists
**our equivalent:** we use manifest files, but could use wildcards

### 3. pattern transformations
```make
# source → object
TEST_TOOL_NET_OBJS = $(TEST_TOOL_NET_SRCS:%.c=o/$(MODE)/%.o)

# test source → test result
TEST_TOOL_NET_TESTS = $(TEST_TOOL_NET_SRCS_TEST:%.c=o/$(MODE)/%.ok)

# lua test → lua test result
TEST_TOOL_NET_LUAS_TEST:%.lua=o/$(MODE)/%.lua.runs
```

**benefit:** systematic mapping from source to build artifacts
**our equivalent:** we already do this!
```make
luatest_files := $(patsubst %,$(luatest_o)/%.ok,$(test_files))
```

### 4. explicit DIRECTDEPS
```make
TEST_TOOL_NET_DIRECTDEPS = \
	LIBC_CALLS \
	LIBC_FMT \
	LIBC_SOCK \
	THIRD_PARTY_SQLITE3

TEST_TOOL_NET_DEPS := \
	$(call uniq,$(foreach x,$(TEST_TOOL_NET_DIRECTDEPS),$($(x))))
```

**this is the key pattern!**

each module declares its **direct** dependencies by name, then expands them:
- `LIBC_CALLS` expands to all files in that module
- `$($(x))` does the variable indirection
- `uniq` removes duplicates
- make handles transitive dependencies automatically

**our equivalent:**
```make
# we could do:
build_DIRECTDEPS = cosmic
test_review_DIRECTDEPS = build

# expand to files:
cosmic_files := $(wildcard lib/cosmic/*.lua)
build_files := $(wildcard lib/build/*.lua) $(cosmic_files)

# test depends on module deps:
o/lib/build/test_review.lua: $(build_files)
```

### 5. pattern rules with module deps
```make
o/$(MODE)/test/tool/net/%.dbg: \
		$(TEST_TOOL_NET_DEPS) \
		$(TEST_TOOL_NET_A) \
		o/$(MODE)/test/tool/net/%.o \
		...
	@$(APELINK)
```

**benefit:** every test in this directory automatically depends on module deps
**our equivalent:**
```make
o/luatest/lib/build/%.ok: \
		$(build_module_deps) \
		lib/build/% \
		o/any/lib/build/%
	$(luatest_runner) ...
```

### 6. per-target customization
```make
o/$(MODE)/test/tool/net/redbean_test.runs: \
		private .PLEDGE = inet

o/$(MODE)/test/tool/net/sqlite_test.runs: \
		private .PLEDGE = flock
```

**benefit:** individual tests can have special settings
**our equivalent:**
```make
$(luatest_o)/lib/build/test_luafiles.lua.ok: TEST_ARGS = $(manifest_files)
```

we already do this!

### 7. checks vs tests
```make
TEST_TOOL_NET_TESTS = $(TEST_TOOL_NET_SRCS_TEST:%.c=o/$(MODE)/%.ok)

TEST_TOOL_NET_CHECKS = \
	$(TEST_TOOL_NET_HDRS:%=o/$(MODE)/%.ok) \
	$(TEST_TOOL_NET_SRCS_TEST:%.c=o/$(MODE)/%.runs)
```

**distinction:**
- `.ok` files: header checks, static analysis
- `.runs` files: actual test execution

**our equivalent:**
- `.ok` files: test results
- could add: `.check.ok` for linting

## applying to our codebase

### current structure
```
lib/
├── build/
│   ├── fetch.lua
│   ├── review.lua
│   └── test_review.lua
└── cosmic/
    └── spawn.lua
```

### cosmopolitan-inspired approach

#### 1. declare module dependencies
```make
# lib/build/cook.mk
build_DIRECTDEPS = cosmic

# lib/cosmic/cook.mk
cosmic_DIRECTDEPS =  # no external deps

# expand deps
cosmic_lua_files := $(wildcard lib/cosmic/*.lua)
build_lua_files := $(wildcard lib/build/*.lua)

# module includes its deps' files
build_ALL_DEPS := $(cosmic_lua_files)
```

#### 2. pattern rules depend on module deps
```make
# all tests in lib/build/ depend on build module deps
o/luatest/lib/build/%.ok: $(build_ALL_DEPS)
```

#### 3. automatic file discovery
```make
# discover all test files
build_test_files := $(wildcard lib/build/test_*.lua)
cosmic_test_files := $(wildcard lib/cosmic/test_*.lua)

# transform to .ok files
build_test_oks := $(build_test_files:lib/%.lua=o/luatest/lib/%.lua.ok)
```

#### 4. copy to o/ with deps
```make
# copy pattern (like .c → .o)
o/any/lib/%.lua: lib/%.lua
	@mkdir -p $(@D)
	cp $< $@

# lib/build files depend on cosmic being built
o/any/lib/build/%.lua: $(cosmic_lua_files:%=o/any/%)
```

### concrete example

```make
# lib/cosmic/cook.mk
cosmic_DIRECTDEPS =
cosmic_lua_files := $(wildcard lib/cosmic/*.lua)
cosmic_ALL_DEPS :=

# lib/build/cook.mk
build_DIRECTDEPS = cosmic
build_lua_files := $(wildcard lib/build/*.lua)
build_ALL_DEPS := $(cosmic_lua_files)

# Makefile - copy to o/
o/any/lib/%.lua: lib/%.lua
	@mkdir -p $(@D)
	cp $< $@

# lib/build files depend on cosmic deps
o/any/lib/build/%.lua: lib/build/%.lua $(build_ALL_DEPS:%=o/any/%)
	@mkdir -p $(@D)
	cp $< $@

# tests depend on their o/ copy (which has all deps)
o/luatest/lib/build/%.ok: lib/build/% o/any/lib/build/%
	$(luatest_runner) $< $@
```

**result:** touching `lib/cosmic/spawn.lua` rebuilds all lib/build files and tests!

## key insights from cosmopolitan

### 1. modules declare direct deps by name
not by file path, by module name:
```make
TEST_TOOL_NET_DIRECTDEPS = LIBC_SOCK THIRD_PARTY_SQLITE3
```

then expand to files later.

### 2. pattern rules + module deps
```make
o/$(MODE)/module/%.o: $(MODULE_DEPS)
```

every file in module automatically depends on module's dependencies.

### 3. separate discovery from transformation
```make
FILES := $(wildcard src/*)        # discover
SRCS := $(filter %.c,$(FILES))    # filter
OBJS := $(SRCS:%.c=o/%.o)         # transform
```

clear pipeline: find → filter → transform

### 4. build artifacts mirror source tree
```
test/tool/net/foo_test.c  →  o/$(MODE)/test/tool/net/foo_test.o
                          →  o/$(MODE)/test/tool/net/foo_test.ok
```

same structure, just in `o/`

### 5. make handles transitive deps
you declare: A depends on B, B depends on C
make figures out: A depends on C

## what we should adopt

### ✓ already doing
- pattern transformations (source → .ok)
- per-target variables (TEST_ARGS, TEST_ENV)
- build tree (`o/`)

### → should adopt
- module-level DIRECTDEPS declarations
- pattern rules with module deps
- systematic o/ mirroring with deps

### ? consider
- wildcard discovery vs manifest files
- MODE for different build types
- separate .ok (static) vs .runs (execution)

## proposed migration

### phase 1: add DIRECTDEPS
```make
# lib/*/cook.mk files declare their deps
build_DIRECTDEPS = cosmic
cosmic_DIRECTDEPS =
```

### phase 2: expand deps to file lists
```make
cosmic_lua_files := $(wildcard lib/cosmic/*.lua)
build_ALL_DEPS := $(cosmic_lua_files)
```

### phase 3: tests depend on module deps
```make
o/luatest/lib/build/%.ok: lib/build/% o/any/lib/build/% $(build_ALL_DEPS)
```

### phase 4: o/ files depend on module deps
```make
o/any/lib/build/%.lua: lib/build/%.lua $(build_ALL_DEPS:%=o/any/%)
```

**result:** cosmopolitan-style dependency tracking!
