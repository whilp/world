modules += tl
tl_version := 3p/tl/version.lua
tl_run := $(o)/bin/run-teal.lua
tl_files := $(tl_run)
tl_tests := $(wildcard 3p/tl/test_*.lua)
tl_deps := argparse cosmos

.PRECIOUS: $(tl_files)
teal_runner := $(bootstrap_cosmic) $(tl_run)
