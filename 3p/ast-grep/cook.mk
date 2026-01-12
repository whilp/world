modules += ast-grep
ast-grep_version := 3p/ast-grep/version.lua
ast-grep_srcs := $(wildcard 3p/ast-grep/*.lua)
ast-grep_run := $(o)/bin/run-astgrep.lua
ast-grep_files := $(ast-grep_run)
ast-grep_tests := $(wildcard 3p/ast-grep/test_*.tl)

.PRECIOUS: $(ast-grep_files)
astgrep_runner := $(bootstrap_cosmic) -- $(ast-grep_run)
