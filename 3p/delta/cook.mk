$(o)/%/3p/delta/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/delta/.extracted: private .INTERNET = 1
$(o)/%/3p/delta/.extracted: private .CPU = 120
$(o)/%/3p/delta/.extracted: 3p/delta/version.lua $(fetch)
	$(lua_bin) $(fetch) $< delta $* $(dir $@)

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/delta/.extracted)
