modules += ast-grep
ast-grep_version := 3p/ast-grep/version.lua
ast-grep_run := $(o)/bin/run-astgrep.lua
ast-grep_report := $(o)/bin/report-astgrep.lua
ast-grep_files := $(ast-grep_run) $(ast-grep_report)
ast-grep_tests := $(wildcard 3p/ast-grep/test_*.lua)

.PRECIOUS: $(ast-grep_files)
astgrep_runner := $(bootstrap_cosmic) $(ast-grep_run)
astgrep_reporter := $(bootstrap_cosmic) $(ast-grep_report)
