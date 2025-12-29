# lib/aerosnap/cook.mk - aerosnap module tests

TEST_STAMPS += o/lib/aerosnap/test.lua.ok

o/lib/aerosnap/test.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rw:/dev/null
o/lib/aerosnap/test.lua.ok: private .PLEDGE = stdio rpath proc exec
o/lib/aerosnap/test.lua.ok: private .CPU = 30
o/lib/aerosnap/test.lua.ok: $(lua_test) lib/aerosnap/test.lua lib/aerosnap/init.lua
	@mkdir -p $(@D)
	$(lua_test) lib/aerosnap/test.lua
	@touch $@
