cosmos-latest: ## update cosmos version file
cosmos-latest: private .PLEDGE = stdio rpath wpath cpath inet dns
cosmos-latest: private .INTERNET = 1
cosmos-latest: $(lua_bin)
	$(lua_bin) lib/cosmos/latest.lua > lib/cosmos/version.lua

.PHONY: cosmos-latest
