# 3p/cosmic-lua - cosmopolitan lua with batteries included

bins += o/%/cosmic-lua/bin/cosmic-lua

# libs to bundle in cosmic-lua
cosmic_lua_lib_dirs := o/any/lib o/any/spawn/lib o/any/walk/lib o/any/cosmic/lib
cosmic_lua_3p_dirs := o/any/luaunit/lib o/%/argparse/lib o/%/lfs/lib

# all lib files needed for cosmic-lua
cosmic_lua_libs := $(lib_libs)
cosmic_lua_libs += o/any/spawn/lib/spawn/init.lua
cosmic_lua_libs += o/any/walk/lib/walk/init.lua
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

# test dependencies
o/any/3p/cosmic-lua/test.lua.luatest.ok: o/$(current_platform)/cosmic-lua/bin/cosmic-lua
o/any/3p/cosmic-lua/test.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/cosmic-lua

.PHONY: cosmic-lua cosmic-lua-all
