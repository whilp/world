# lib/cook.mk - core lib tests

o/test/lib-whereami.ok: private .UNVEIL = r:lib rx:$(lua_test) rw:/dev/null
o/test/lib-whereami.ok: private .PLEDGE = stdio rpath proc exec
o/test/lib-whereami.ok: private .CPU = 30
o/test/lib-whereami.ok: $(lua_test) lib/test_whereami.lua lib/whereami.lua lib/spawn/init.lua
	@mkdir -p $(@D)
	$(lua_test) lib/test_whereami.lua
	@touch $@

o/test/lib-daemonize.ok: private .UNVEIL = r:lib rx:$(lua_test) rwc:/tmp rw:/dev/null
o/test/lib-daemonize.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/test/lib-daemonize.ok: private .CPU = 30
o/test/lib-daemonize.ok: $(lua_test) lib/test_daemonize.lua lib/daemonize.lua lib/spawn/init.lua
	@mkdir -p $(@D)
	$(lua_test) lib/test_daemonize.lua
	@touch $@
