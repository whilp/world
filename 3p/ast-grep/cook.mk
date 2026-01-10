modules += ast-grep
ast-grep_version := 3p/ast-grep/version.lua
ast-grep_srcs := $(wildcard 3p/ast-grep/*.lua)
ast-grep_run := $(o)/bin/run-astgrep.lua
ast-grep_files := $(ast-grep_run)
ast-grep_tests := $(wildcard 3p/ast-grep/test_*.lua)

.PRECIOUS: $(ast-grep_files)
astgrep_runner := $(bootstrap_cosmic) -- $(ast-grep_run)

# Override staging to move binaries to bin/ subdirectory
# Note: sg is a launcher that requires ast-grep in PATH, so symlink sg -> ast-grep
$(o)/ast-grep/.staged: $(o)/ast-grep/.fetched
	@$(build_stage) $$(readlink $(o)/ast-grep/.versioned) $(platform) $< $@.tmp
	@versioned_dir=$$(readlink -f $@.tmp) && \
		mkdir -p $$versioned_dir/bin && \
		mv $$versioned_dir/ast-grep $$versioned_dir/bin/ && \
		rm -f $$versioned_dir/sg && \
		ln -s ast-grep $$versioned_dir/bin/sg
	@mv $@.tmp $@
