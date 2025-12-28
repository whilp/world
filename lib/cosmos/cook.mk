cosmos-latest: ## Fetch latest cosmos version
cosmos-latest: private .PLEDGE = stdio rpath wpath cpath inet dns
cosmos-latest: private .INTERNET = 1
cosmos-latest: $(lua_bin)
	$(lib_lua) lib/cosmos/latest.lua

.PHONY: cosmos-latest
