include lib/whereami/cook.mk
include lib/daemonize/cook.mk
include lib/claude/cook.mk
include lib/cosmos/cook.mk
include lib/nvim/cook.mk
include lib/environ/cook.mk
include lib/build/cook.mk
include lib/aerosnap/cook.mk
include lib/work/cook.mk
# lib/spawn/cook.mk included via lib/home/cook.mk
# 3p/lua/cook.mk included via Makefile

# TEST_STAMPS accumulated from cook.mk files via +=
test: $(TEST_STAMPS) ## Run all tests

# check for orphaned test files not in TEST_STAMPS
check-test-coverage:
	@echo "Checking for orphaned test files..."
	@rg --files -g 'test*.lua' lib 3p | grep -v -e test_lib.lua -e setup/test.lua -e 'lib/work/test.lua' | sort > /tmp/test_files.txt
	@echo '$(TEST_STAMPS)' | tr ' ' '\n' | sed 's|^o/\(.*\)\.ok$$|\1|' | sort > /tmp/test_stamps.txt
	@if ! diff -q /tmp/test_files.txt /tmp/test_stamps.txt > /dev/null 2>&1; then \
		orphans=$$(comm -23 /tmp/test_files.txt /tmp/test_stamps.txt); \
		if [ -n "$$orphans" ]; then \
			echo "ERROR: Test files not in TEST_STAMPS:"; \
			echo "$$orphans" | sed 's/^/  /'; \
			rm -f /tmp/test_files.txt /tmp/test_stamps.txt; \
			exit 1; \
		fi; \
	fi
	@rm -f /tmp/test_files.txt /tmp/test_stamps.txt
	@echo "All test files are wired up."

.PHONY: test check-test-coverage
