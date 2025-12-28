claude-latest: private .PLEDGE = stdio rpath wpath cpath inet dns
claude-latest: private .INTERNET = 1
claude-latest: $(lua_bin)
	$(lib_lua) 3p/claude/latest.lua

.PHONY: claude-latest
