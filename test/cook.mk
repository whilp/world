modules += test
test_run := $(o)/test/run.lua
test_files := $(test_run)
test_deps := $(bootstrap_files)
test_tests := $(wildcard test/test_*.lua)

test_runner := $(bootstrap_cosmic) $(test_run)

export PATH := $(o)/test:$(PATH)
