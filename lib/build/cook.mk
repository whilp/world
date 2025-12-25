test-build-download-tool: private .UNVEIL = r:lib/build r:lib r:3p rx:$(lua_bin) r:$(test_runner) r:$(CURDIR) rwc:/tmp rw:/dev/null
test-build-download-tool: private .PLEDGE = stdio rpath wpath cpath proc exec
test-build-download-tool: private .CPU = 60
test-build-download-tool: lua
	cd lib/build && HOME=$(CURDIR) $(home_lua) $(CURDIR)/$(test_runner) test.lua

.PHONY: test-build-download-tool
