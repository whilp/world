modules += test
test_run := $(o)/bin/run-test.lua
astgrep_run := $(o)/bin/run-astgrep.lua
test_files := $(test_run) $(astgrep_run)
test_tests := $(wildcard lib/test/test_*.lua)

.PRECIOUS: $(test_files)
test_runner := $(bootstrap_cosmic) $(test_run)
astgrep_runner := $(bootstrap_cosmic) $(astgrep_run)

