test_runner := lib/run-test.lua

include lib/cook.mk
include lib/claude/cook.mk
include lib/nvim/cook.mk
include lib/environ/cook.mk
include lib/build/cook.mk

TEST_TARGETS := test-3p-lua test-lib-whereami test-home test-lib-daemonize \
                test-claude test-nvim test-claude-skills test-environ test-build-download-tool

test-all: $(TEST_TARGETS)

test: private .UNVEIL = r:$(CURDIR) rx:$(lua_release_bin) rw:/dev/null
test: private .PLEDGE = stdio rpath wpath cpath proc exec
test: private .CPU = 120
test: lua-release
	$(lua_release_bin) $(test_runner) 3p/lua/test.lua
	$(lua_release_bin) $(test_runner) lib/test_whereami.lua
	$(lua_release_bin) $(test_runner) lib/home/test_main.lua
	$(lua_release_bin) $(test_runner) lib/test_daemonize.lua
	$(lua_release_bin) $(test_runner) lib/claude/test.lua
	$(lua_release_bin) $(test_runner) lib/nvim/test.lua
	$(lua_release_bin) $(test_runner) lib/claude/test_skills.lua
	$(lua_release_bin) $(test_runner) lib/environ/test.lua
	$(lua_release_bin) $(test_runner) lib/build/test.lua

.PHONY: test-all test
