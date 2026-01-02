# lib/cosmic - cosmopolitan lua utilities namespace

cosmic_lib := $(o_any)/cosmic/lib
cosmic := $(o_any)/cosmic/bin/cosmic

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

$(luatest_o)/lib/cosmic/test_spawn.lua.ok: $(cosmic_lib)/cosmic/spawn.lua
$(luatest_o)/lib/cosmic/test_spawn.lua.ok: TEST_ENV = TEST_BIN_DIR=$(o_platform)/cosmos

$(luatest_o)/lib/cosmic/test_walk.lua.ok: $(cosmic_lib)/cosmic/walk.lua

$(luatest_o)/lib/cosmic/test_binary.lua.ok: $(cosmic)
$(luatest_o)/lib/cosmic/test_binary.lua.ok: TEST_ENV = TEST_BIN_DIR=$(o_any)/cosmic

# cosmic binary build
cosmic_lib_dirs := $(o_any)/lib $(cosmic_lib)
cosmic_libs := $(lib_libs)

$(cosmic): $(o_platform)/cosmos/bin/lua $(o_platform)/cosmos/bin/zip $(cosmic_libs) $(luaunit) $(o_platform)/argparse/lib/argparse.lua $(o_platform)/lfs/lib/lfs.lua
	rm -rf $(o_any)/cosmic/staging
	mkdir -p $(o_any)/cosmic/staging/.lua $(@D)
	$(foreach d,$(cosmic_lib_dirs),cp -r $(d)/* $(o_any)/cosmic/staging/.lua/;)
	cp -r $(o_any)/luaunit/lib/* $(o_any)/cosmic/staging/.lua/
	cp -r $(o_platform)/argparse/lib/* $(o_any)/cosmic/staging/.lua/
	cp -r $(o_platform)/lfs/lib/* $(o_any)/cosmic/staging/.lua/
	cp $(o_platform)/cosmos/bin/lua $@
	chmod +x $@
	cd $(o_any)/cosmic/staging && $(CURDIR)/$(o_platform)/cosmos/bin/zip -qr $(CURDIR)/$@ .lua

cosmic: $(cosmic) ## Build cosmic for current platform

.PHONY: cosmic
