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

test-lib-daemonize: private .UNVEIL = \
	r:.local/lib/lua \
	rx:$(lua_bin) \
	r:$(test_runner) \
	r:$(CURDIR) \
	rwc:/tmp \
	rw:/dev/null
test-lib-daemonize: lua
	cd .local/lib/lua && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/.local/lib/lua/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test_daemonize.lua

test-claude: private .UNVEIL = \
	r:src/claude \
	r:.local/lib/lua \
	rx:$(lua_bin) \
	r:$(test_runner) \
	r:$(CURDIR) \
	rwc:/tmp \
	rw:/dev/null
test-claude: lua
	cd src/claude && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/.local/lib/lua/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test.lua

test-nvim: private .UNVEIL = \
	r:src/nvim \
	r:.local/lib/lua \
	rx:$(lua_bin) \
	r:$(test_runner) \
	r:$(CURDIR) \
	rwc:/tmp \
	rw:/dev/null
test-nvim: lua
	cd src/nvim && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/.local/lib/lua/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test.lua

test-claude-skills: private .UNVEIL = \
	r:src/claude \
	r:.local/lib/lua \
	r:.claude/skills \
	rx:$(lua_bin) \
	r:$(test_runner) \
	r:$(CURDIR) \
	rw:/dev/null
test-claude-skills: lua
	cd src/claude && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/.local/lib/lua/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test_skills.lua

# skip test-work until work.lua module is available in CI
test-all: test-3p-lua test-lib-whereami test-home test-lib-daemonize test-claude test-nvim test-claude-skills

.PHONY: test-3p-lua test-lib-whereami test-work test-home test-lib-daemonize test-claude test-nvim test-claude-skills test-all
