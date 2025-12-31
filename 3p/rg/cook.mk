$(o)/%/3p/rg/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/rg/.extracted: private .INTERNET = 1
$(o)/%/3p/rg/.extracted: private .CPU = 120
$(o)/%/3p/rg/.extracted: 3p/rg/version.lua $(fetch)
	@mkdir -p $(dir $@)
	$(lua_bin) $(fetch) $< rg $* $(dir $@)
	touch $@

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/rg/.extracted)
