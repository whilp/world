# lib/environ/cook.mk - environ module

TEST_STAMPS += o/lib/environ/test.lua.ok

o/lib/environ/test.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rw:/dev/null
o/lib/environ/test.lua.ok: private .PLEDGE = stdio rpath proc exec
o/lib/environ/test.lua.ok: private .CPU = 30
o/lib/environ/test.lua.ok: $(lua_test) lib/environ/test.lua lib/environ/init.lua
	@mkdir -p $(@D)
	$(lua_test) lib/environ/test.lua
	@touch $@
