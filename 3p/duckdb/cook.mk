modules += duckdb
duckdb_version := 3p/duckdb/version.lua
duckdb_tests := 3p/duckdb/test_duckdb.tl

# Override staging to move binary to bin/ subdirectory
$(o)/duckdb/.staged: $(o)/duckdb/.fetched
	@$(build_stage) $$(readlink $(o)/duckdb/.versioned) $(platform) $< $@.tmp
	@versioned_dir=$$(readlink -f $@.tmp) && \
		mkdir -p $$versioned_dir/bin && \
		mv $$versioned_dir/duckdb $$versioned_dir/bin/ 2>/dev/null || true
	@mv $@.tmp $@
