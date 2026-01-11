modules += delta
delta_version := 3p/delta/version.lua
delta_tests := 3p/delta/test_delta.tl

# Override staging to move binary to bin/ subdirectory
$(o)/delta/.staged: $(o)/delta/.fetched
	@$(build_stage) $$(readlink $(o)/delta/.versioned) $(platform) $< $@.tmp
	@versioned_dir=$$(readlink -f $@.tmp) && \
		mkdir -p $$versioned_dir/bin && \
		mv $$versioned_dir/delta $$versioned_dir/bin/ 2>/dev/null || true
	@mv $@.tmp $@
