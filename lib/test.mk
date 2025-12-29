include lib/cook.mk
include lib/claude/cook.mk
include lib/cosmos/cook.mk
include lib/nvim/cook.mk
include lib/environ/cook.mk
include lib/build/cook.mk

TEST_STAMPS := \
	o/test/3p-lua-modules.ok \
	o/test/3p-lua-funcs.ok \
	o/test/lib-whereami.ok \
	o/test/lib-daemonize.ok \
	o/test/home.ok \
	o/test/claude.ok \
	o/test/claude-skills.ok \
	o/test/nvim.ok \
	o/test/environ.ok \
	o/test/build-download-tool.ok

test: $(TEST_STAMPS) ## Run all tests

# convenience aliases (phony targets that depend on stamps)
test-3p-lua-modules: o/test/3p-lua-modules.ok
test-3p-lua-funcs: o/test/3p-lua-funcs.ok
test-lib-whereami: o/test/lib-whereami.ok
test-lib-daemonize: o/test/lib-daemonize.ok
test-home: o/test/home.ok
test-claude: o/test/claude.ok
test-claude-skills: o/test/claude-skills.ok
test-nvim: o/test/nvim.ok
test-environ: o/test/environ.ok
test-build-download-tool: o/test/build-download-tool.ok

.PHONY: test test-3p-lua-modules test-3p-lua-funcs test-lib-whereami test-lib-daemonize test-home \
        test-claude test-claude-skills test-nvim test-environ test-build-download-tool
