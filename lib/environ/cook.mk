test-environ: private .UNVEIL = r:lib/environ r:lib rx:$(lua_bin) r:$(test_runner) r:$(CURDIR) rw:/dev/null
test-environ: private .PLEDGE = stdio rpath proc exec
test-environ: private .CPU = 30
test-environ: lua
	cd lib/environ && HOME=$(CURDIR) $(home_lua) $(CURDIR)/$(test_runner) test.lua

.PHONY: test-environ
