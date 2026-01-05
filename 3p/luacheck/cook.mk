modules += luacheck
luacheck_version := 3p/luacheck/version.lua
luacheck_srcs := $(wildcard 3p/luacheck/*.lua)
luacheck_run := $(o)/bin/run-luacheck.lua
luacheck_files := $(luacheck_run)
luacheck_tests := 3p/luacheck/test_luacheck.lua
luacheck_deps := argparse

.PRECIOUS: $(luacheck_files)
luacheck_runner := $(bootstrap_cosmic) -- $(luacheck_run)
