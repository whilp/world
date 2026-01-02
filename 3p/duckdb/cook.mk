duckdb_version := 3p/duckdb/version.lua
bins += o/%/duckdb/bin/duckdb

o/any/3p/duckdb/test.lua.luatest.ok: o/$(current_platform)/duckdb/bin/duckdb
o/any/3p/duckdb/test.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/duckdb

o/%/duckdb/archive.zip: $(duckdb_version) $(fetch)
	$(fetch) $(duckdb_version) $* $@

o/%/duckdb/staging/duckdb: $(duckdb_version) $(extract) o/%/duckdb/archive.zip
	$(extract) $(duckdb_version) $* o/$*/duckdb/archive.zip o/$*/duckdb/staging

o/%/duckdb/bin/duckdb: $(duckdb_version) $(install) o/%/duckdb/staging/duckdb
	$(install) $(duckdb_version) $* o/$*/duckdb bin o/$*/duckdb/staging/duckdb
