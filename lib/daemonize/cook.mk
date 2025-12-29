# lib/daemonize/cook.mk - daemonize module

o/lib/daemonize/test.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rwc:/tmp rw:/dev/null
o/lib/daemonize/test.lua.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/lib/daemonize/test.lua.ok: private .CPU = 30
o/lib/daemonize/test.lua.ok: $(lua_test) lib/daemonize/test.lua lib/daemonize/init.lua lib/spawn/init.lua
	@mkdir -p $(@D)
	$(lua_test) lib/daemonize/test.lua
	@touch $@
