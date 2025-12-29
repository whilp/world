# lib/cook.mk - core lib tests

o/test/lib-whereami.ok: private .UNVEIL = r:lib rx:$(lua_test) r:$(test_runner) r:$(CURDIR) rw:/dev/null
o/test/lib-whereami.ok: private .PLEDGE = stdio rpath proc exec
o/test/lib-whereami.ok: private .CPU = 30
o/test/lib-whereami.ok: $(lua_test) $(test_runner) lib/test_whereami.lua lib/whereami.lua
	@mkdir -p $(@D)
	cd lib && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_test) $(CURDIR)/$(test_runner) test_whereami.lua
	@touch $@

o/test/lib-daemonize.ok: private .UNVEIL = r:lib rx:$(lua_test) r:$(test_runner) r:$(CURDIR) rwc:/tmp rw:/dev/null
o/test/lib-daemonize.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/test/lib-daemonize.ok: private .CPU = 30
o/test/lib-daemonize.ok: $(lua_test) $(test_runner) lib/test_daemonize.lua lib/daemonize.lua lib/spawn/init.lua
	@mkdir -p $(@D)
	cd lib && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_test) $(CURDIR)/$(test_runner) test_daemonize.lua
	@touch $@
