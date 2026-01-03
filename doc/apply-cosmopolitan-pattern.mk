# applying cosmopolitan pattern to our lua tests

# ============================================================================
# module dependency declarations (in each lib/*/cook.mk)
# ============================================================================

# lib/cosmic/cook.mk
cosmic_DIRECTDEPS =  # no external deps
cosmic_files := $(wildcard lib/cosmic/*.lua)

# lib/build/cook.mk
build_DIRECTDEPS = cosmic
build_files := $(wildcard lib/build/*.lua)

# lib/work/cook.mk
work_DIRECTDEPS = cosmic
work_files := $(wildcard lib/work/*.lua)

# ============================================================================
# expand module deps to file lists
# ============================================================================

# helper to expand module name to its files
# $(call module_files,cosmic) -> lib/cosmic/*.lua
module_files = $($(1)_files)

# expand DIRECTDEPS to actual file lists
# for each module in DIRECTDEPS, get its files
cosmic_ALL_DEPS :=
build_ALL_DEPS := $(foreach dep,$(build_DIRECTDEPS),$($(dep)_files))
work_ALL_DEPS := $(foreach dep,$(work_DIRECTDEPS),$($(dep)_files))

# ============================================================================
# copy source to o/ (like .c → .o)
# ============================================================================

# base pattern: copy any lua file
o/any/lib/%.lua: lib/%.lua
	@mkdir -p $(@D)
	cp $< $@

# ============================================================================
# o/ files depend on their module's deps
# ============================================================================

# lib/build files depend on cosmic deps being built
o/any/lib/build/%.lua: $(build_ALL_DEPS:%=o/any/%)

# lib/work files depend on cosmic deps being built
o/any/lib/work/%.lua: $(work_ALL_DEPS:%=o/any/%)

# ============================================================================
# tests depend on o/ copy (which has all transitive deps)
# ============================================================================

# test depends on:
# 1. source file (for running)
# 2. o/ copy (for dependency tracking)
$(luatest_o)/lib/%.ok: lib/% o/any/lib/%
	$(TEST_ENV) $(luatest_runner) $< $@ $(TEST_ARGS)

# ============================================================================
# example: full dependency chain
# ============================================================================

# touch lib/cosmic/spawn.lua
# → o/any/lib/cosmic/spawn.lua needs rebuild (copy rule)
# → o/any/lib/build/review.lua depends on it (build_ALL_DEPS)
# → o/luatest/lib/build/test_review.lua.ok depends on o/any/lib/build/review.lua
# → test reruns!

# verification:
# $ touch lib/cosmic/spawn.lua
# $ make -n o/luatest/lib/build/test_review.lua.ok
# should show:
# 1. cp lib/cosmic/spawn.lua o/any/lib/cosmic/spawn.lua
# 2. cp lib/build/review.lua o/any/lib/build/review.lua
# 3. run test

# ============================================================================
# benefits
# ============================================================================

# 1. declare deps once per module:
#    build_DIRECTDEPS = cosmic
#
# 2. make handles transitive:
#    test_review → review → spawn (automatic!)
#
# 3. systematic:
#    every test in lib/build/ gets cosmic deps
#
# 4. greppable:
#    $ rg DIRECTDEPS
#    finds all module dependencies
#
# 5. same pattern as cosmopolitan:
#    proven in large codebase

# ============================================================================
# comparison with current approach
# ============================================================================

# current (manual, per-test):
# $(luatest_o)/lib/build/test_review.lua.ok: $(o_any)/build/lib/build/review.lua
# (missing: review.lua → spawn.lua transitive dep)

# cosmopolitan pattern (module-level, automatic):
# build_DIRECTDEPS = cosmic
# o/any/lib/build/%.lua: $(build_ALL_DEPS:%=o/any/%)
# (transitive deps automatic via make)

# ============================================================================
# migration path
# ============================================================================

# step 1: add DIRECTDEPS declarations to cook.mk files
# step 2: add module_files variables
# step 3: add ALL_DEPS expansion
# step 4: change test rule to depend on o/any/lib/%
# step 5: remove manual .ok deps from cook.mk files

# ============================================================================
# open questions
# ============================================================================

# Q: should we copy test files to o/ or run from source?
# A: cosmopolitan runs tests from o/ (test.c → o/test → o/test.ok)
#    we could do: lib/test.lua → o/lib/test.lua → o/luatest/lib/test.ok
#    or keep:    lib/test.lua → o/luatest/lib/test.ok (run from source)
#
#    recommend: run from source, o/ just for dependency tracking

# Q: how to handle require() that aren't in DIRECTDEPS?
# A: builtin modules (cosmo.unix) don't need deps
#    missing deps will cause test failures (require() fails)
#    fix by adding to DIRECTDEPS

# Q: performance cost of copying?
# A: lua files are small (~1-10KB)
#    parallel make handles this well
#    can measure with: time make -j8

# Q: can we auto-generate DIRECTDEPS?
# A: yes! scan require() calls:
#    $ grep 'require("' lib/build/*.lua | extract module names
#    generate: build_DIRECTDEPS = cosmic environ
#    cosmopolitan does this manually (more explicit)
