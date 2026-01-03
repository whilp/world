modules += test
test_run := $(o)/bin/test-run.lua
test_files := $(test_run)
test_tests := $(wildcard lib/test/test_*.lua)

test_runner := $(bootstrap_cosmic) $(test_run)
$(test_run): lib/test/run.lua

export PATH := $(o)/test:$(PATH)
.PRECIOUS: $(test_run)
