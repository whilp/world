# lib/cosmic - cosmopolitan lua utilities namespace

lib_lua_modules += cosmic
lib_dirs += o/any/cosmic/lib
lib_libs += o/any/cosmic/lib/cosmic/init.lua
lib_libs += o/any/cosmic/lib/cosmic/spawn.lua
lib_libs += o/any/cosmic/lib/cosmic/walk.lua
lib_libs += o/any/cosmic/lib/cosmic/help.lua

# for compatibility with scripts that reference spawn_lib/walk_lib
spawn_lib := o/any/cosmic/lib/cosmic/spawn.lua
walk_lib := o/any/cosmic/lib/cosmic/walk.lua

o/any/cosmic/lib/cosmic/%.lua: lib/cosmic/%.lua
	mkdir -p $(@D)
	cp $< $@

# test dependencies
$(luatest_o)/lib/cosmic/test_cosmic.lua.ok: o/any/cosmic/lib/cosmic/init.lua
$(luatest_o)/lib/cosmic/test_cosmic.lua.ok: o/any/cosmic/lib/cosmic/spawn.lua
$(luatest_o)/lib/cosmic/test_cosmic.lua.ok: o/any/cosmic/lib/cosmic/walk.lua
$(luatest_o)/lib/cosmic/test_cosmic.lua.ok: o/any/cosmic/lib/cosmic/help.lua

$(luatest_o)/lib/cosmic/test_spawn.lua.ok: o/any/cosmic/lib/cosmic/spawn.lua o/$(current_platform)/cosmos/bin/lua
$(luatest_o)/lib/cosmic/test_spawn.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/cosmos

$(luatest_o)/lib/cosmic/test_walk.lua.ok: o/any/cosmic/lib/cosmic/walk.lua

$(luatest_o)/lib/cosmic/test_binary.lua.ok: o/$(current_platform)/cosmic-lua/bin/cosmic-lua
$(luatest_o)/lib/cosmic/test_binary.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/cosmic-lua

# cosmic-lua binary build
bins += o/%/cosmic-lua/bin/cosmic-lua

cosmic_lua_lib_dirs := o/any/lib o/any/cosmic/lib
cosmic_lua_libs := $(lib_libs)
cosmic_lua_libs += o/any/cosmic/lib/cosmic/init.lua
cosmic_lua_libs += o/any/cosmic/lib/cosmic/spawn.lua
cosmic_lua_libs += o/any/cosmic/lib/cosmic/walk.lua
cosmic_lua_libs += o/any/cosmic/lib/cosmic/help.lua

o/%/cosmic-lua/bin/cosmic-lua: o/%/cosmos/bin/lua o/%/cosmos/bin/zip $(cosmic_lua_libs) $(luaunit) o/%/argparse/lib/argparse.lua o/%/lfs/lib/lfs.lua
	rm -rf o/$*/cosmic-lua/staging
	mkdir -p o/$*/cosmic-lua/staging/.lua $(@D)
	$(foreach d,$(cosmic_lua_lib_dirs),cp -r $(d)/* o/$*/cosmic-lua/staging/.lua/;)
	cp -r o/any/luaunit/lib/* o/$*/cosmic-lua/staging/.lua/
	cp -r o/$*/argparse/lib/* o/$*/cosmic-lua/staging/.lua/
	cp -r o/$*/lfs/lib/* o/$*/cosmic-lua/staging/.lua/
	cp o/$*/cosmos/bin/lua $@
	chmod +x $@
	cd o/$*/cosmic-lua/staging && $(CURDIR)/o/$*/cosmos/bin/zip -qr $(CURDIR)/$@ .lua

cosmic-lua: o/$(current_platform)/cosmic-lua/bin/cosmic-lua ## Build cosmic-lua for current platform

cosmic-lua-all: $(foreach p,$(platforms),o/$(p)/cosmic-lua/bin/cosmic-lua) ## Build cosmic-lua for all platforms

.PHONY: cosmic-lua cosmic-lua-all
