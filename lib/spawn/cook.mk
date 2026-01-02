lib_lua_modules += spawn
lib_dirs += o/any/spawn/lib
spawn_lib := o/any/spawn/lib/spawn/init.lua
lib_libs += $(spawn_lib)

$(spawn_lib): lib/spawn/init.lua
	mkdir -p $(@D)
	cp $< $@

o/any/lib/spawn/test_spawn.lua.luatest.ok: $(spawn_lib) o/$(current_platform)/cosmos/bin/lua
o/any/lib/spawn/test_spawn.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/cosmos
