# lib/whereami/cook.mk - whereami module

o/lib/whereami/test.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rw:/dev/null
o/lib/whereami/test.lua.ok: private .PLEDGE = stdio rpath proc exec
o/lib/whereami/test.lua.ok: private .CPU = 30
o/lib/whereami/test.lua.ok: $(lua_test) lib/whereami/test.lua lib/whereami/init.lua lib/spawn/init.lua
	@mkdir -p $(@D)
	$(lua_test) lib/whereami/test.lua
	@touch $@
