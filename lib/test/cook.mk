modules += test
test_run := $(o)/bin/run-test.lua
test_report := $(o)/bin/report-test.lua
test_files := $(test_run) $(test_report)
test_tests := $(wildcard lib/test/test_*.lua)

.PRECIOUS: $(test_files)
test_runner := $(bootstrap_cosmic) $(test_run)
test_reporter := $(bootstrap_cosmic) $(test_report)

