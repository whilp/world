test_runner := .local/lib/lua/run-test.lua

test-3p-lua: lua
	cd 3p/lua && $(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test.lua

test-lib-whereami: lua
	cd .local/lib/lua && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/.local/lib/lua/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test_whereami.lua

test-work: lua
	cd .local/lib/lua/test/work && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/.local/lib/lua/?.lua;$(CURDIR)/.local/lib/lua/test/work/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) run.lua

test-home: lua
	cd home && LUA_PATH="$(CURDIR)/.local/lib/lua/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test_main.lua

test-all: test-3p-lua test-lib-whereami test-work test-home

.PHONY: test-3p-lua test-lib-whereami test-work test-home test-all
