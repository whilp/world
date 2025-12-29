spawn_dir = lib/spawn
spawn_sources = $(filter-out %test%.lua,$(wildcard $(spawn_dir)/*.lua))

TEST_STAMPS += o/lib/spawn/test_spawn.lua.ok

o/lib/spawn/test_spawn.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rwc:/tmp rw:/dev/null
o/lib/spawn/test_spawn.lua.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/lib/spawn/test_spawn.lua.ok: private .CPU = 30
o/lib/spawn/test_spawn.lua.ok: $(lua_test) lib/spawn/test_spawn.lua lib/spawn/init.lua
	@mkdir -p $(@D)
	$(lua_test) lib/spawn/test_spawn.lua
	@touch $@
