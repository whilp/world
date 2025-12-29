# lib/build/cook.mk - build tools

o/test/build-download-tool.ok: private .UNVEIL = r:lib/build r:lib r:3p rx:$(lua_test) r:$(test_runner) r:$(CURDIR) rwc:/tmp rw:/dev/null
o/test/build-download-tool.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/test/build-download-tool.ok: private .CPU = 60
o/test/build-download-tool.ok: $(lua_test) $(test_runner) lib/build/test.lua lib/build/download-tool.lua
	@mkdir -p $(@D)
	cd lib/build && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;;" \
		$(CURDIR)/$(lua_test) $(CURDIR)/$(test_runner) test.lua
	@touch $@
