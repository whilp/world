# lib/nvim/cook.mk - nvim module

o/test/nvim.ok: private .UNVEIL = r:lib/nvim r:lib rx:$(lua_test) r:$(test_runner) r:$(CURDIR) rwc:/tmp rw:/dev/null
o/test/nvim.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/test/nvim.ok: private .CPU = 60
o/test/nvim.ok: $(lua_test) $(test_runner) lib/nvim/test.lua lib/nvim/main.lua
	@mkdir -p $(@D)
	cd lib/nvim && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;;" \
		$(CURDIR)/$(lua_test) $(CURDIR)/$(test_runner) test.lua
	@touch $@
