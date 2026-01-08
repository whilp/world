modules += test
test_srcs := $(wildcard lib/test/*.lua)
test_run := $(o)/bin/run-test.lua
test_files := $(test_run)
test_tests := $(wildcard lib/test/test_*.lua)

.PRECIOUS: $(test_files)
# sandboxed test runner
test_runner := $(bootstrap_cosmic) $(build_sandbox) test $(test_run)

