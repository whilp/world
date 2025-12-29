# lib/claude/cook.mk - claude module

claude-latest:
claude-latest: private .PLEDGE = stdio rpath wpath cpath inet dns
claude-latest: private .INTERNET = 1
claude-latest: $(lua_bin)
	$(lua_bin) lib/claude/latest.lua

.PHONY: claude-latest

o/lib/claude/test.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rwc:/tmp rw:/dev/null
o/lib/claude/test.lua.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/lib/claude/test.lua.ok: private .CPU = 60
o/lib/claude/test.lua.ok: $(lua_test) lib/claude/test.lua lib/claude/main.lua
	@mkdir -p $(@D)
	$(lua_test) lib/claude/test.lua
	@touch $@
