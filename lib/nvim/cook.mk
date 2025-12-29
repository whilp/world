# lib/nvim/cook.mk - nvim module

o/test/nvim.ok: private .UNVEIL = r:lib rx:$(lua_test) rwc:/tmp rw:/dev/null
o/test/nvim.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/test/nvim.ok: private .CPU = 60
o/test/nvim.ok: $(lua_test) lib/nvim/test.lua lib/nvim/main.lua
	@mkdir -p $(@D)
	LUA_PATH="lib/?.lua;lib/?/init.lua;;" $(lua_test) lib/nvim/test.lua
	@touch $@
