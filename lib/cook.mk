test-lib-whereami: private .UNVEIL = r:lib rx:$(lua_bin) r:$(test_runner) r:$(CURDIR) rw:/dev/null
test-lib-whereami: private .PLEDGE = stdio rpath proc exec
test-lib-whereami: private .CPU = 30
test-lib-whereami: lua
	cd lib && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test_whereami.lua

test-lib-daemonize: private .UNVEIL = r:lib rx:$(lua_bin) r:$(test_runner) r:$(CURDIR) rwc:/tmp rw:/dev/null
test-lib-daemonize: private .PLEDGE = stdio rpath wpath cpath proc exec
test-lib-daemonize: private .CPU = 30
test-lib-daemonize: lua
	cd lib && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test_daemonize.lua

test-work: private .UNVEIL = r:lib r:/home/codespace/.local/bootstrap/lib rx:$(lua_bin) r:$(test_runner) r:$(CURDIR) rwc:lib/test/work rw:/dev/null
test-work: private .PLEDGE = stdio rpath wpath cpath proc exec
test-work: private .CPU = 60
test-work: lua
	cd lib/test/work && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/lib/?.lua;$(CURDIR)/lib/test/work/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) run.lua

.PHONY: test-lib-whereami test-lib-daemonize test-work
