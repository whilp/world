lib_lua_modules += spawn
lib_dirs += o/any/spawn/lib
lib_libs += o/any/spawn/lib/spawn/init.lua

o/any/spawn/lib/spawn/init.lua: lib/spawn/init.lua
	mkdir -p $(@D)
	cp $< $@

o/any/lib/spawn/test_spawn.lua.luatest.ok: o/any/spawn/lib/spawn/init.lua o/$(current_platform)/cosmos/bin/lua
o/any/lib/spawn/test_spawn.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/cosmos
