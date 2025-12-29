# lib/claude/cook.mk - claude module

lib_lua = LUA_PATH="$(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;;" $(CURDIR)/$(lua_bin)

claude-latest:
claude-latest: private .PLEDGE = stdio rpath wpath cpath inet dns
claude-latest: private .INTERNET = 1
claude-latest: $(lua_bin)
	$(lib_lua) lib/claude/latest.lua

.PHONY: claude-latest

o/test/claude.ok: private .UNVEIL = r:lib/claude r:lib rx:$(lua_test) r:$(test_runner) r:$(CURDIR) rwc:/tmp rw:/dev/null
o/test/claude.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/test/claude.ok: private .CPU = 60
o/test/claude.ok: $(lua_test) $(test_runner) lib/claude/test.lua lib/claude/main.lua
	@mkdir -p $(@D)
	cd lib/claude && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_test) $(CURDIR)/$(test_runner) test.lua
	@touch $@

o/test/claude-skills.ok: private .UNVEIL = r:lib/claude r:lib r:.claude/skills rx:$(lua_test) r:$(test_runner) r:$(CURDIR) rw:/dev/null
o/test/claude-skills.ok: private .PLEDGE = stdio rpath proc exec
o/test/claude-skills.ok: private .CPU = 30
o/test/claude-skills.ok: $(lua_test) $(test_runner) lib/claude/test_skills.lua lib/claude/main.lua $(wildcard .claude/skills/*.md)
	@mkdir -p $(@D)
	cd lib/claude && HOME=$(CURDIR) LUA_PATH="$(CURDIR)/lib/?.lua;;" \
		$(CURDIR)/$(lua_test) $(CURDIR)/$(test_runner) test_skills.lua
	@touch $@
