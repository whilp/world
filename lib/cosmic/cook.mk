# lib/cosmic - cosmopolitan lua utilities namespace

cosmic_lib := $(o_any)/cosmic/lib

lib_lua_modules += cosmic
lib_dirs += $(cosmic_lib)
lib_libs += $(cosmic_lib)/cosmic/init.lua
lib_libs += $(cosmic_lib)/cosmic/spawn.lua
lib_libs += $(cosmic_lib)/cosmic/walk.lua
lib_libs += $(cosmic_lib)/cosmic/help.lua

$(cosmic_lib)/cosmic/%.lua: lib/cosmic/%.lua
	mkdir -p $(@D)
	cp $< $@

# test dependencies
$(luatest_o)/lib/cosmic/test_cosmic.lua.ok: $(lib_libs)

$(luatest_o)/lib/cosmic/test_spawn.lua.ok: $(cosmic_lib)/cosmic/spawn.lua o/$(current_platform)/cosmos/bin/lua
$(luatest_o)/lib/cosmic/test_spawn.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/cosmos

$(luatest_o)/lib/cosmic/test_walk.lua.ok: $(cosmic_lib)/cosmic/walk.lua

$(luatest_o)/lib/cosmic/test_binary.lua.ok: o/$(current_platform)/cosmic/bin/cosmic
$(luatest_o)/lib/cosmic/test_binary.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/cosmic

# cosmic binary build
bins += o/%/cosmic/bin/cosmic

cosmic_lib_dirs := $(o_any)/lib $(cosmic_lib)
cosmic_libs := $(lib_libs)

o/%/cosmic/bin/cosmic: o/%/cosmos/bin/lua o/%/cosmos/bin/zip $(cosmic_libs) $(luaunit) o/%/argparse/lib/argparse.lua o/%/lfs/lib/lfs.lua
	rm -rf o/$*/cosmic/staging
	mkdir -p o/$*/cosmic/staging/.lua $(@D)
	$(foreach d,$(cosmic_lib_dirs),cp -r $(d)/* o/$*/cosmic/staging/.lua/;)
	cp -r $(o_any)/luaunit/lib/* o/$*/cosmic/staging/.lua/
	cp -r o/$*/argparse/lib/* o/$*/cosmic/staging/.lua/
	cp -r o/$*/lfs/lib/* o/$*/cosmic/staging/.lua/
	cp o/$*/cosmos/bin/lua $@
	chmod +x $@
	cd o/$*/cosmic/staging && $(CURDIR)/o/$*/cosmos/bin/zip -qr $(CURDIR)/$@ .lua

cosmic: o/$(current_platform)/cosmic/bin/cosmic ## Build cosmic for current platform

.PHONY: cosmic
