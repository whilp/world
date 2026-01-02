duckdb_version := 3p/duckdb/version.lua
bins += o/%/duckdb/bin/duckdb

$(luatest_o)/3p/duckdb/test.lua.ok: o/$(current_platform)/duckdb/bin/duckdb
$(luatest_o)/3p/duckdb/test.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/duckdb

o/%/duckdb/archive.zip: $(duckdb_version) $(fetch)
	$(fetch) $(duckdb_version) $* $@

o/%/duckdb/staging/duckdb: $(duckdb_version) $(extract) o/%/duckdb/archive.zip
	$(extract) $(duckdb_version) $* o/$*/duckdb/archive.zip o/$*/duckdb/staging

o/%/duckdb/bin/duckdb: $(duckdb_version) $(install) o/%/duckdb/staging/duckdb
	$(install) $(duckdb_version) $* o/$*/duckdb bin o/$*/duckdb/staging/duckdb
