modules += tl
tl_version := 3p/tl/version.lua
tl_run := $(o)/bin/run-teal.lua
tl_report := $(o)/bin/report-teal.lua
tl_files := $(o)/bin/tl $(o)/lib/tl.lua $(tl_run) $(tl_report)
tl_tests := $(wildcard 3p/tl/test_*.lua)
tl_deps := argparse cosmos

.PRECIOUS: $(tl_files)
teal_runner := $(bootstrap_cosmic) $(tl_run)
teal_reporter := $(bootstrap_cosmic) $(tl_report)
