modules += ruff
ruff_version := 3p/ruff/version.lua
ruff_tests := 3p/ruff/test_ruff.lua

# Override staging to move binary to bin/ subdirectory
$(o)/ruff/.staged: $(o)/ruff/.fetched
	@$(build_stage) $$(readlink $(o)/ruff/.versioned) $(platform) $< $@.tmp
	@versioned_dir=$$(readlink -f $@.tmp) && \
		mkdir -p $$versioned_dir/bin && \
		mv $$versioned_dir/ruff $$versioned_dir/bin/ 2>/dev/null || true
	@mv $@.tmp $@
