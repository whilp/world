$(o)/%/3p/gh/bin/gh: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/gh/bin/gh: private .INTERNET = 1
$(o)/%/3p/gh/bin/gh: private .CPU = 120
$(o)/%/3p/gh/bin/gh: 3p/gh/version.lua $(fetch)
	$(lua_bin) $(fetch) $< gh $* $(patsubst %/bin/,%/,$(dir $@))

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/gh/bin/gh)
