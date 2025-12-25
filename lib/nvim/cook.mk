test-nvim: private .UNVEIL = r:lib/nvim r:lib rx:$(lua_bin) r:$(test_runner) r:$(CURDIR) rwc:/tmp rw:/dev/null
test-nvim: private .PLEDGE = stdio rpath wpath cpath proc exec
test-nvim: private .CPU = 60
test-nvim: lua
	cd lib/nvim && HOME=$(CURDIR) $(home_lua) $(CURDIR)/$(test_runner) test.lua

.PHONY: test-nvim
