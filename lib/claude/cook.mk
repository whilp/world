# lib/claude/cook.mk - claude module

lib_lua = LUA_PATH="lib/?.lua;lib/?/init.lua;;" $(lua_bin)

claude-latest:
claude-latest: private .PLEDGE = stdio rpath wpath cpath inet dns
claude-latest: private .INTERNET = 1
claude-latest: $(lua_bin)
	$(lib_lua) lib/claude/latest.lua

.PHONY: claude-latest

o/test/claude.ok: private .UNVEIL = r:lib rx:$(lua_test) rwc:/tmp rw:/dev/null
o/test/claude.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/test/claude.ok: private .CPU = 60
o/test/claude.ok: $(lua_test) lib/claude/test.lua lib/claude/main.lua
	@mkdir -p $(@D)
	LUA_PATH="lib/?.lua;lib/?/init.lua;;" $(lua_test) lib/claude/test.lua
	@touch $@

o/test/claude-skills.ok: private .UNVEIL = r:lib r:.claude/skills rx:$(lua_test) rw:/dev/null
o/test/claude-skills.ok: private .PLEDGE = stdio rpath proc exec
o/test/claude-skills.ok: private .CPU = 30
o/test/claude-skills.ok: $(lua_test) lib/claude/test_skills.lua lib/claude/main.lua $(wildcard .claude/skills/*.md)
	@mkdir -p $(@D)
	LUA_PATH="lib/?.lua;lib/?/init.lua;;" $(lua_test) lib/claude/test_skills.lua
	@touch $@
