# lib/build/cook.mk - build tools

o/test/build-download-tool.ok: private .UNVEIL = r:lib r:3p rx:$(lua_test) rwc:/tmp rw:/dev/null
o/test/build-download-tool.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/test/build-download-tool.ok: private .CPU = 60
o/test/build-download-tool.ok: $(lua_test) lib/build/test.lua lib/build/download-tool.lua
	@mkdir -p $(@D)
	LUA_PATH="lib/?.lua;lib/?/init.lua;;" $(lua_test) lib/build/test.lua
	@touch $@
