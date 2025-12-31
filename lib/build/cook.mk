# lib/build/cook.mk - build tools

o/lib/build/test.lua.ok: private .UNVEIL = r:lib r:3p rx:$(lua_test) rwc:/tmp rw:/dev/null
o/lib/build/test.lua.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/lib/build/test.lua.ok: private .CPU = 60
o/lib/build/test.lua.ok: $(lua_test) lib/build/test.lua lib/build/download-tool.lua
	@mkdir -p $(@D)
	$(lua_test) lib/build/test.lua
	@touch $@
