modules += luacheck
luacheck_version := 3p/luacheck/version.lua
luacheck_run := $(o)/bin/run-luacheck.lua
luacheck_report := $(o)/bin/report-luacheck.lua
luacheck_files := $(luacheck_run) $(luacheck_report)
luacheck_tests := 3p/luacheck/test_luacheck.lua
luacheck_deps := argparse

.PRECIOUS: $(luacheck_files)
luacheck_runner := $(bootstrap_cosmic) $(luacheck_run)
luacheck_reporter := $(bootstrap_cosmic) $(luacheck_report)
