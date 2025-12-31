$(o)/%/3p/duckdb/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/duckdb/.extracted: private .INTERNET = 1
$(o)/%/3p/duckdb/.extracted: private .CPU = 120
$(o)/%/3p/duckdb/.extracted: 3p/duckdb/version.lua $(fetch)
	@mkdir -p $(dir $@)
	$(lua_bin) $(fetch) $< duckdb $* $(dir $@)
	touch $@

duckdb_binaries := $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/duckdb/.extracted)
