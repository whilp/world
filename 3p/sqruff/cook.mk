$(o)/%/3p/sqruff/bin/sqruff: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/sqruff/bin/sqruff: private .INTERNET = 1
$(o)/%/3p/sqruff/bin/sqruff: private .CPU = 120
$(o)/%/3p/sqruff/bin/sqruff: 3p/sqruff/version.lua $(fetch)
	$(lua_bin) $(fetch) $< sqruff $* $(patsubst %/bin/,%/,$(dir $@))

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/sqruff/bin/sqruff)
