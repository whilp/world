# lib/nvim/cook.mk - nvim module

o/lib/nvim/test.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rwc:/tmp rw:/dev/null
o/lib/nvim/test.lua.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/lib/nvim/test.lua.ok: private .CPU = 60
o/lib/nvim/test.lua.ok: $(lua_test) lib/nvim/test.lua lib/nvim/main.lua
	@mkdir -p $(@D)
	$(lua_test) lib/nvim/test.lua
	@touch $@
