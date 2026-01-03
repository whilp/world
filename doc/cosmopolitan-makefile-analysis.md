# key patterns from cosmopolitan's main Makefile

## 1. include order matters - phased build

```make
include libc/nexgen32e/BUILD.mk     #─┐
include libc/sysv/BUILD.mk          # ├──SYSTEM SUPPORT
include libc/nt/BUILD.mk            # │  You can do math
include libc/intrin/BUILD.mk        # │  You can use the stack
# ...                                #─┘
include libc/calls/BUILD.mk         #─┐
include libc/runtime/BUILD.mk       # ├──SYSTEMS RUNTIME
# ...                                # │  You can issue system calls
include libc/stdio/BUILD.mk         #─┐
# ...                                # ├──DYNAMIC RUNTIME
                                    # │  You can now use stdio
```

**insight:** includes are ordered by dependency layers
- system support → runtime → stdio → networking
- each layer builds on previous
- **our equivalent:** we could order lib/cosmic before lib/build

## 2. PKGS aggregation pattern

```make
PKGS =

# each BUILD.mk adds to PKGS:
# PKGS += TEST_TOOL_NET

# aggregate all objects/sources across packages:
OBJS  = $(foreach x,$(PKGS),$($(x)_OBJS))
SRCS := $(foreach x,$(PKGS),$($(x)_SRCS))
HDRS := $(foreach x,$(PKGS),$($(x)_HDRS))
BINS  = $(foreach x,$(PKGS),$($(x)_BINS))
TESTS = $(foreach x,$(PKGS),$($(x)_TESTS))
```

**this is the key aggregation pattern!**

each BUILD.mk:
1. declares a package: `PKGS += FOO`
2. defines variables: `FOO_SRCS`, `FOO_OBJS`, `FOO_DEPS`
3. main Makefile aggregates them all

**our equivalent:**
```make
MODULES =

# lib/cosmic/cook.mk:
MODULES += cosmic
cosmic_files := $(wildcard lib/cosmic/*.lua)
cosmic_tests := $(wildcard lib/cosmic/test_*.lua)

# lib/build/cook.mk:
MODULES += build
build_files := $(wildcard lib/build/*.lua)
build_tests := $(wildcard lib/build/test_*.lua)

# Makefile aggregates:
all_lua_files := $(foreach x,$(MODULES),$($(x)_files))
all_test_files := $(foreach x,$(MODULES),$($(x)_tests))
```

## 3. module dependency expansion

```make
# in BUILD.mk:
TEST_TOOL_NET_DIRECTDEPS = \
	LIBC_CALLS \
	LIBC_SOCK

# expand to actual dependencies:
TEST_TOOL_NET_DEPS := \
	$(call uniq,$(foreach x,$(TEST_TOOL_NET_DIRECTDEPS),$($(x))))
```

**pattern:**
- `DIRECTDEPS` = list of module NAMES
- `DEPS` = expanded to actual files via `$($(x))`
- `$(LIBC_CALLS)` expands to all files in LIBC_CALLS module

**our equivalent:**
```make
# lib/build/cook.mk:
build_DIRECTDEPS = cosmic

# expand (in Makefile or cook.mk):
build_DEPS := $(foreach x,$(build_DIRECTDEPS),$($(x)_files))
# build_DEPS now contains all cosmic_files
```

## 4. o/$(MODE) build tree

```make
MODE ?= opt  # or dbg, rel, tiny

o/$(MODE)/test/tool/net/%.o: test/tool/net/%.c
o/$(MODE)/test/tool/net/%.ok: o/$(MODE)/test/tool/net/%
```

**insight:** MODE is a build variant, not platform
- o/dbg/ for debug builds
- o/opt/ for optimized builds
- o/tiny/ for minimal size

**our equivalent:** we use platform
- o/linux-x86_64/
- o/darwin-arm64/

could also support:
- o/dbg/linux-x86_64/
- o/opt/linux-x86_64/

## 5. wildcard + filter pattern

```make
# discover all files
FILES := $(wildcard test/tool/net/*)

# filter by type
SRCS = $(filter %.c,$(FILES))
SRCS_TEST = $(filter %_test.c,$(SRCS))
HDRS = $(filter %.h,$(FILES))

# transform to build artifacts
OBJS = $(SRCS:%.c=o/$(MODE)/%.o)
TESTS = $(SRCS_TEST:%.c=o/$(MODE)/%.ok)
```

**our current approach:** manifest files (explicit lists)
**cosmopolitan approach:** wildcards (discovery)

**trade-off:**
- wildcards: automatic, but includes everything
- manifest: explicit, but needs updating

**we could do:**
```make
build_FILES := $(wildcard lib/build/*)
build_SRCS := $(filter %.lua,$(build_FILES))
build_TESTS := $(filter test_%.lua,$(build_SRCS))
```

## 6. depend file generation

```make
o/$(MODE)/depend: o/$(MODE)/srcs.txt o/$(MODE)/hdrs.txt
	$(MKDEPS) -o $@ @o/$(MODE)/srcs.txt @o/$(MODE)/hdrs.txt

-include o/$(MODE)/depend
```

**cosmopolitan uses a tool to generate dependencies!**
- `mkdeps` scans C source files
- generates make dependency rules
- included at bottom of Makefile

**similar to what we discussed:**
- scan lua files for require()
- generate .deps files
- include them

## 7. .DEFAULT rule for cleanup

```make
.DEFAULT:
	@$(ECHO) NOTE: deleting o/$(MODE)/depend because of unspecified prerequisite: $@
	$(RM) o/$(MODE)/depend
```

**insight:** when make can't find a rule for a file
- it's probably stale (file was deleted)
- solution: delete dependency graph and regenerate

**clever!** handles source file deletion automatically

## 8. COSMOPOLITAN package definition

```make
COSMOPOLITAN = \
	LIBC_CALLS \
	LIBC_SOCK \
	LIBC_STDIO \
	THIRD_PARTY_ZLIB

o/$(MODE)/cosmopolitan.a: \
	$(call reverse,$(call uniq,$(foreach x,$(COSMOPOLITAN),$($(x)))))
```

**pattern:**
- define high-level aggregate (COSMOPOLITAN)
- list all component modules
- expand to all files via foreach + indirection

**our equivalent:**
```make
LUA_RUNTIME = \
	cosmic \
	build \
	work

o/any/lua-runtime.a: \
	$(foreach x,$(LUA_RUNTIME),$($(x)_files))
```

## 9. toolchain artifacts

```make
TOOLCHAIN_ARTIFACTS = \
	o/$(MODE)/cosmopolitan.a \
	o/$(MODE)/tool/build/apelink

.PHONY: toolchain
toolchain: $(TOOLCHAIN_ARTIFACTS)
```

**pattern:** define meta-targets that aggregate artifacts

**our equivalent:**
```make
LUA_TOOLCHAIN = \
	o/any/lua/bin/lua \
	o/$(platform)/cosmos/bin/lua

.PHONY: bootstrap
bootstrap: $(LUA_TOOLCHAIN)
```

## 10. per-mode configuration

```make
ifeq ($(MODE),tiny)
CFLAGS += -Os -ffunction-sections
endif

ifeq ($(MODE),dbg)
CFLAGS += -O0 -g
endif
```

**we could do:**
```make
ifeq ($(MODE),test)
TEST_ENV = VERBOSE=1
endif

ifeq ($(MODE),coverage)
luatest_runner = coverage run $(luatest_script)
endif
```

## applying to our project

### pattern 1: MODULES aggregation
```make
# each cook.mk:
MODULES += cosmic
cosmic_files := $(wildcard lib/cosmic/*.lua)

# Makefile:
all_files := $(foreach x,$(MODULES),$($(x)_files))
```

### pattern 2: DIRECTDEPS expansion
```make
# cook.mk:
build_DIRECTDEPS = cosmic
build_DEPS := $(foreach x,$(build_DIRECTDEPS),$($(x)_files))
```

### pattern 3: o/ tree with deps
```make
# copy to o/
o/any/lib/%.lua: lib/%.lua
	cp $< $@

# module files depend on their DIRECTDEPS being built
o/any/lib/build/%.lua: $(build_DEPS:%=o/any/%)
```

### pattern 4: wildcard discovery
```make
cosmic_FILES := $(wildcard lib/cosmic/*)
cosmic_SRCS := $(filter %.lua,$(cosmic_FILES))
cosmic_TESTS := $(filter test_%.lua,$(cosmic_SRCS))
```

### pattern 5: depend generation
```make
o/any/depend: $(lua_files)
	lua scan-requires.lua $(lua_files) > $@

-include o/any/depend
```

## key differences from our current approach

| aspect | cosmopolitan | our current | proposed |
|--------|--------------|-------------|----------|
| discovery | wildcards | manifest files | wildcards or manifest |
| deps | DIRECTDEPS + expansion | manual per-test | DIRECTDEPS + expansion |
| build tree | o/$(MODE)/ | o/$(platform)/ | o/any/ or o/$(platform)/ |
| aggregation | PKGS list | test_files variable | MODULES list |
| dep tracking | mkdeps tool | manual | could use scanner |

## what to adopt

### definitely adopt:
1. **DIRECTDEPS pattern** - module deps by name
2. **foreach expansion** - `$(foreach x,$(DEPS),$($(x)_files))`
3. **o/ tree with deps** - your original proposal!

### consider:
1. **wildcard discovery** - instead of manifest
2. **MODULES aggregation** - instead of single lists
3. **depend generation** - scan require() calls

### maybe later:
1. **MODE support** - test/debug/release builds
2. **.DEFAULT cleanup** - auto-regenerate deps
3. **phased includes** - order by dependency layers

## bottom line

cosmopolitan's Makefile confirms your intuition:
- **o/ tree approach** ✓ (they do o/$(MODE)/)
- **module DIRECTDEPS** ✓ (they use this everywhere)
- **pattern rules + deps** ✓ (systematic, not manual)
- **aggregation via foreach** ✓ (PKGS pattern)

the main Makefile is simpler than you might think:
- includes all BUILD.mk files
- aggregates all PKGS
- provides high-level targets (all, check, test)
- dependency graph in o/$(MODE)/depend

each BUILD.mk does the real work:
- declares its package
- defines DIRECTDEPS
- expands to DEPS
- provides pattern rules
