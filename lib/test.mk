test_runner := lib/run-test.lua

test-3p-lua: private .UNVEIL = \
	r:3p/lua \
	rx:$(lua_bin) \
	r:$(test_runner) \
	rwc:3p/lua/o \
	rw:/dev/null
test-3p-lua: lua
	cd 3p/lua && $(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test.lua

test-lib-whereami: private .UNVEIL = \
	r:lib \
	rx:$(lua_bin) \
	r:$(test_runner) \
	r:$(CURDIR) \
	rw:/dev/null
test-lib-whereami: lua
	cd lib && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test_whereami.lua

test-work: private .UNVEIL = \
	r:lib \
	r:/home/codespace/.local/bootstrap/lib \
	rx:$(lua_bin) \
	r:$(test_runner) \
	r:$(CURDIR) \
	rwc:lib/test/work \
	rw:/dev/null
test-work: lua
	cd lib/test/work && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/lib/?.lua;$(CURDIR)/lib/test/work/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) run.lua

test-home: private .UNVEIL = \
	r:lib/home \
	r:lib \
	rx:$(lua_bin) \
	r:$(test_runner) \
	rwc:lib/home/o \
	rw:/dev/null
test-home: lua
	cd lib/home && LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test_main.lua

test-lib-daemonize: private .UNVEIL = \
	r:lib \
	rx:$(lua_bin) \
	r:$(test_runner) \
	r:$(CURDIR) \
	rwc:/tmp \
	rw:/dev/null
test-lib-daemonize: lua
	cd lib && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test_daemonize.lua

test-claude: private .UNVEIL = \
	r:lib/claude \
	r:lib \
	rx:$(lua_bin) \
	r:$(test_runner) \
	r:$(CURDIR) \
	rwc:/tmp \
	rw:/dev/null
test-claude: lua
	cd lib/claude && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test.lua

test-nvim: private .UNVEIL = \
	r:lib/nvim \
	r:lib \
	rx:$(lua_bin) \
	r:$(test_runner) \
	r:$(CURDIR) \
	rwc:/tmp \
	rw:/dev/null
test-nvim: lua
	cd lib/nvim && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test.lua

test-claude-skills: private .UNVEIL = \
	r:lib/claude \
	r:lib \
	r:.claude/skills \
	rx:$(lua_bin) \
	r:$(test_runner) \
	r:$(CURDIR) \
	rw:/dev/null
test-claude-skills: lua
	cd lib/claude && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test_skills.lua

test-environ: private .UNVEIL = \
	r:lib/environ \
	r:lib \
	rx:$(lua_bin) \
	r:$(test_runner) \
	r:$(CURDIR) \
	rw:/dev/null
test-environ: lua
	cd lib/environ && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test.lua

# skip test-work until work.lua module is available in CI
test-all: test-3p-lua test-lib-whereami test-home test-lib-daemonize test-claude test-nvim test-claude-skills test-environ

.PHONY: test-3p-lua test-lib-whereami test-work test-home test-lib-daemonize test-claude test-nvim test-claude-skills test-environ test-all
