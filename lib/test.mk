include lib/whereami/cook.mk
include lib/daemonize/cook.mk
include lib/claude/cook.mk
include lib/cosmos/cook.mk
include lib/nvim/cook.mk
include lib/environ/cook.mk
include lib/build/cook.mk
include lib/aerosnap/cook.mk
include lib/work/cook.mk

TEST_STAMPS += o/lib/home/test_main.lua.ok
TEST_STAMPS += o/lib/claude/test.lua.ok
TEST_STAMPS += o/lib/claude/test_skills.lua.ok
TEST_STAMPS += o/lib/nvim/test.lua.ok
TEST_STAMPS += o/lib/environ/test.lua.ok
TEST_STAMPS += o/lib/build/test.lua.ok
TEST_STAMPS += o/lib/spawn/test_spawn.lua.ok

test: $(TEST_STAMPS) ## Run all tests

check-test-coverage:
	@rg --files -g 'test*.lua' lib 3p | grep -v -e test_lib.lua -e setup/test.lua -e 'lib/work/test.lua' | sort > /tmp/test_files.txt
	@for f in $$(cat /tmp/test_files.txt); do \
		stamp="o/$${f}.ok"; \
		if ! grep -q "$$stamp" lib/test.mk lib/*/cook.mk 3p/*/cook.mk 2>/dev/null; then \
			echo "MISSING: $$f not in any cook.mk"; \
		fi; \
	done
	@rm -f /tmp/test_files.txt

.PHONY: test check-test-coverage
