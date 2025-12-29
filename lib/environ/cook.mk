# lib/environ/cook.mk - environ module

o/test/environ.ok: private .UNVEIL = r:lib rx:$(lua_test) rw:/dev/null
o/test/environ.ok: private .PLEDGE = stdio rpath proc exec
o/test/environ.ok: private .CPU = 30
o/test/environ.ok: $(lua_test) lib/environ/test.lua lib/environ/init.lua
	@mkdir -p $(@D)
	$(lua_test) lib/environ/test.lua
	@touch $@
