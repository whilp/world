# lib/environ/cook.mk - environ module

o/test/environ.ok: private .UNVEIL = r:lib/environ r:lib rx:$(lua_test) r:$(test_runner) r:$(CURDIR) rw:/dev/null
o/test/environ.ok: private .PLEDGE = stdio rpath proc exec
o/test/environ.ok: private .CPU = 30
o/test/environ.ok: $(lua_test) $(test_runner) lib/environ/test.lua lib/environ/init.lua
	@mkdir -p $(@D)
	cd lib/environ && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;;" \
		$(CURDIR)/$(lua_test) $(CURDIR)/$(test_runner) test.lua
	@touch $@
