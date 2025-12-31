$(o)/%/3p/delta/bin/delta: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/delta/bin/delta: private .INTERNET = 1
$(o)/%/3p/delta/bin/delta: private .CPU = 120
$(o)/%/3p/delta/bin/delta: 3p/delta/version.lua $(fetch)
	$(lua_bin) $(fetch) $< delta $* $(patsubst %/bin/,%/,$(dir $@))

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/delta/bin/delta)
