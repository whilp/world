test_runner := .local/lib/lua/run-test.lua

test-3p-lua: private .UNVEIL = \
	r:3p/lua \
	rx:$(lua_bin) \
	r:$(test_runner) \
	rwc:3p/lua/o \
	rw:/dev/null
test-3p-lua: lua
	cd 3p/lua && $(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test.lua

test-lib-whereami: private .UNVEIL = \
	r:.local/lib/lua \
	rx:$(lua_bin) \
	r:$(test_runner) \
	r:$(CURDIR) \
	rw:/dev/null
test-lib-whereami: lua
	cd .local/lib/lua && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/.local/lib/lua/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test_whereami.lua

test-work: private .UNVEIL = \
	r:.local/lib/lua \
	r:/home/codespace/.local/bootstrap/lib \
	rx:$(lua_bin) \
	r:$(test_runner) \
	r:$(CURDIR) \
	rwc:.local/lib/lua/test/work \
	rw:/dev/null
test-work: lua
	cd .local/lib/lua/test/work && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/.local/lib/lua/?.lua;$(CURDIR)/.local/lib/lua/test/work/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) run.lua

test-home: private .UNVEIL = \
	r:home \
	r:.local/lib/lua \
	rx:$(lua_bin) \
	r:$(test_runner) \
	rwc:home/o \
	rw:/dev/null
test-home: lua
	cd home && LUA_PATH="$(CURDIR)/.local/lib/lua/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test_main.lua

test-all: test-3p-lua test-lib-whereami test-work test-home

.PHONY: test-3p-lua test-lib-whereami test-work test-home test-all
