lib_lua_modules += spawn
lib_dirs += o/any/spawn/lib
spawn_lib := o/any/spawn/lib/spawn/init.lua
lib_libs += $(spawn_lib)

$(spawn_lib): lib/spawn/init.lua
	mkdir -p $(@D)
	cp $< $@

$(luatest_o)/lib/spawn/test_spawn.lua.ok: $(spawn_lib) o/$(current_platform)/cosmos/bin/lua
$(luatest_o)/lib/spawn/test_spawn.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/cosmos
