modules += test
test_srcs := $(wildcard lib/test/*.tl)
test_run := $(o)/bin/run-test.lua
test_check_coverage := $(o)/bin/check-coverage.lua
test_files := $(test_run) $(test_check_coverage)
test_tests := $(wildcard lib/test/test_*.tl)

.PRECIOUS: $(test_files)
test_runner := $(bootstrap_cosmic) $(test_run)
coverage_checker := $(bootstrap_cosmic) $(test_check_coverage)

# TODO: replace with cosmic --compile when it supports transpile-only mode
$(test_run): lib/test/run-test.tl | $(bootstrap_files)
	@mkdir -p $(@D)
	@$(bootstrap_cosmic) /zip/tl-gen.lua -- $< -o $@
	@{ echo '#!/usr/bin/env lua'; cat $@; } > $@.tmp && mv $@.tmp $@
	@chmod +x $@

