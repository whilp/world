test_runner := lib/run-test.lua

include lib/cook.mk
include lib/claude/cook.mk
include lib/cosmos/cook.mk
include lib/nvim/cook.mk
include lib/environ/cook.mk
include lib/build/cook.mk

TEST_TARGETS := test-3p-lua test-lib-whereami test-home test-lib-daemonize \
                test-claude test-nvim test-claude-skills test-environ test-build-download-tool

test-all: $(TEST_TARGETS)

test: test-all ## Run all tests

.PHONY: test-all test
