$(o)/%/3p/rg/bin/rg: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/rg/bin/rg: private .INTERNET = 1
$(o)/%/3p/rg/bin/rg: private .CPU = 120
$(o)/%/3p/rg/bin/rg: 3p/rg/version.lua $(fetch)
	$(lua_bin) $(fetch) $< rg $* $(patsubst %/bin/,%/,$(dir $@))

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/rg/bin/rg)
