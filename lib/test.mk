include lib/cook.mk
include lib/claude/cook.mk
include lib/cosmos/cook.mk
include lib/nvim/cook.mk
include lib/environ/cook.mk
include lib/build/cook.mk
include lib/aerosnap/cook.mk
include lib/work/cook.mk
# lib/spawn/cook.mk included via lib/home/cook.mk

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
	o/lib/spawn/test_spawn.lua.ok \
	o/lib/aerosnap/test.lua.ok \
	o/lib/work/test_backup.lua.ok \
	o/lib/work/test_blocked_on_display.lua.ok \
	o/lib/work/test_blockers.lua.ok \
	o/lib/work/test_command_blocked.lua.ok \
	o/lib/work/test_file_locking.lua.ok \
	o/lib/work/test_orphaned_blocks.lua.ok \
	o/lib/work/test_string_sanitization.lua.ok \
	o/lib/work/test_validate_blocks.lua.ok

test: $(TEST_STAMPS) ## Run all tests

# check for orphaned test files not in TEST_STAMPS
check-test-coverage:
	@echo "Checking for orphaned test files..."
	@find lib 3p -name 'test*.lua' -type f | grep -v -e test_lib.lua -e setup/test.lua | sort > /tmp/test_files.txt
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
