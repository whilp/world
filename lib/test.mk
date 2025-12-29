include lib/cook.mk
include lib/claude/cook.mk
include lib/cosmos/cook.mk
include lib/nvim/cook.mk
include lib/environ/cook.mk
include lib/build/cook.mk

TEST_STAMPS := \
	o/3p/lua/test_modules.lua.ok \
	o/3p/lua/test_funcs.lua.ok \
	o/lib/test_whereami.lua.ok \
	o/lib/test_daemonize.lua.ok \
	o/lib/home/test_main.lua.ok \
	o/lib/claude/test.lua.ok \
	o/lib/claude/test_skills.lua.ok \
	o/lib/nvim/test.lua.ok \
	o/lib/environ/test.lua.ok \
	o/lib/build/test.lua.ok \
	o/lib/spawn/test_spawn.lua.ok

test: $(TEST_STAMPS) ## Run all tests

.PHONY: test
