modules += ast-grep
ast-grep_version := 3p/ast-grep/version.lua
ast-grep_run := $(o)/bin/run-astgrep.lua
ast-grep_report := $(o)/bin/report-astgrep.lua
ast-grep_files := $(ast-grep_run) $(ast-grep_report)
ast-grep_tests := $(wildcard 3p/ast-grep/test_*.lua)

.PRECIOUS: $(ast-grep_files)
astgrep_runner := $(bootstrap_cosmic) $(ast-grep_run)
astgrep_reporter := $(bootstrap_cosmic) $(ast-grep_report)

# Override staging to move binaries to bin/ subdirectory
$(o)/ast-grep/.staged: $(o)/ast-grep/.fetched
	@$(build_stage) $$(readlink $(o)/ast-grep/.versioned) $(platform) $< $@.tmp
	@versioned_dir=$$(readlink -f $@.tmp) && \
		mkdir -p $$versioned_dir/bin && \
		mv $$versioned_dir/ast-grep $$versioned_dir/sg $$versioned_dir/bin/ 2>/dev/null || true
	@mv $@.tmp $@
