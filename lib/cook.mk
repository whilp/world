# lib/cook.mk - core lib tests

o/lib/test_whereami.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rw:/dev/null
o/lib/test_whereami.lua.ok: private .PLEDGE = stdio rpath proc exec
o/lib/test_whereami.lua.ok: private .CPU = 30
o/lib/test_whereami.lua.ok: $(lua_test) lib/test_whereami.lua lib/whereami.lua lib/spawn/init.lua
	@mkdir -p $(@D)
	$(lua_test) lib/test_whereami.lua
	@touch $@

o/lib/test_daemonize.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rwc:/tmp rw:/dev/null
o/lib/test_daemonize.lua.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/lib/test_daemonize.lua.ok: private .CPU = 30
o/lib/test_daemonize.lua.ok: $(lua_test) lib/test_daemonize.lua lib/daemonize.lua lib/spawn/init.lua
	@mkdir -p $(@D)
	$(lua_test) lib/test_daemonize.lua
	@touch $@
