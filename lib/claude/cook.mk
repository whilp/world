claude-latest: private .PLEDGE = stdio rpath wpath cpath inet dns
claude-latest: private .INTERNET = 1
claude-latest: $(lua_bin)
	$(lib_lua) lib/claude/latest.lua

.PHONY: claude-latest

test-claude: private .UNVEIL = r:lib/claude r:lib rx:$(lua_bin) r:$(test_runner) r:$(CURDIR) rwc:/tmp rw:/dev/null
test-claude: private .PLEDGE = stdio rpath wpath cpath proc exec
test-claude: private .CPU = 60
test-claude: lua
	cd lib/claude && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test.lua

test-claude-skills: private .UNVEIL = r:lib/claude r:lib r:.claude/skills rx:$(lua_bin) r:$(test_runner) r:$(CURDIR) rw:/dev/null
test-claude-skills: private .PLEDGE = stdio rpath proc exec
test-claude-skills: private .CPU = 30
test-claude-skills: lua
	cd lib/claude && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test_skills.lua

.PHONY: test-claude test-claude-skills
