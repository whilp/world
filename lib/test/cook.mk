modules += test
test_run := $(o)/bin/run-test.lua
test_files := $(test_run)
test_tests := $(wildcard lib/test/test_*.lua)

.PRECIOUS: $(test_run)
test_runner := $(bootstrap_cosmic) $(test_run)

