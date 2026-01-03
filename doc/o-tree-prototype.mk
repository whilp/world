# prototype: o/ tree approach for test dependencies

# existing pattern (we already do this for some files)
o/any/lib/build/%.lua: lib/build/%.lua
	@mkdir -p $(@D)
	cp $< $@

# extend to all lib files
o/any/lib/%.lua: lib/%.lua
	@mkdir -p $(@D)
	cp $< $@

# declare module dependencies (this is the key!)
# these are the direct require() relationships

# lib/cosmic module (no external deps)
# (files depend on themselves via pattern rule above)

# lib/build module uses cosmic
o/any/lib/build/review.lua: o/any/lib/cosmic/spawn.lua
o/any/lib/build/manifest.lua: o/any/lib/cosmic/spawn.lua o/any/lib/cosmic/walk.lua
o/any/lib/build/luacheck.lua: o/any/lib/cosmic/spawn.lua
o/any/lib/build/ast-grep.lua: o/any/lib/cosmic/spawn.lua

# tests depend on what they import
o/any/lib/build/test_review.lua: o/any/lib/build/review.lua
o/any/lib/build/test_fetch.lua: o/any/lib/build/fetch.lua
o/any/lib/build/test_install.lua: o/any/lib/build/install.lua

# test run depends on o/ copy (which has all transitive deps)
$(luatest_o)/%.ok: o/any/% %
	$(TEST_ENV) $(luatest_runner) $(word 2,$^) $@ $(TEST_ARGS)

# example: what does test_review depend on?
# test_review.lua.ok needs:
#   1. o/any/lib/build/test_review.lua (first prereq)
#   2. lib/build/test_review.lua (second prereq, actual test file)
#
# o/any/lib/build/test_review.lua needs:
#   1. lib/build/test_review.lua (source)
#   2. o/any/lib/build/review.lua (import)
#
# o/any/lib/build/review.lua needs:
#   1. lib/build/review.lua (source)
#   2. o/any/lib/cosmic/spawn.lua (import)
#
# o/any/lib/cosmic/spawn.lua needs:
#   1. lib/cosmic/spawn.lua (source)
#
# make builds the entire tree automatically!

# benefits:
# - declare direct deps once
# - make handles transitive
# - touch lib/cosmic/spawn.lua → rebuilds test_review.ok ✓

# alternative: tests depend on o/ copy directly
$(luatest_o)/%.ok: o/any/%
	$(TEST_ENV) $(luatest_runner) $< $@ $(TEST_ARGS)

# in this version:
# - test file copied to o/
# - test run from o/ copy
# - LUA_PATH points to o/any/lib
# - all requires work from o/

# hybrid: test from source, deps from o/
$(luatest_o)/lib/%.ok: lib/% o/any/lib/%
	$(TEST_ENV) $(luatest_runner) $< $@ $(TEST_ARGS)

# in this version:
# - test runs from source (lib/)
# - but depends on o/any/lib/% (which pulls in all deps)
# - o/any/lib/% is just a marker showing deps are built
# - LUA_PATH includes both source and o/

# to use this prototype:
# 1. include this file in Makefile
# 2. add direct dependency declarations to cook.mk files
# 3. LUA_PATH should include o/any/lib
# 4. tests automatically get transitive deps
