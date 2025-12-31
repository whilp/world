$(o)/%/3p/duckdb/bin/duckdb: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/duckdb/bin/duckdb: private .INTERNET = 1
$(o)/%/3p/duckdb/bin/duckdb: private .CPU = 120
$(o)/%/3p/duckdb/bin/duckdb: 3p/duckdb/version.lua $(fetch)
	$(lua_bin) $(fetch) $< duckdb $* $(patsubst %/bin/,%/,$(dir $@))

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/duckdb/bin/duckdb)
