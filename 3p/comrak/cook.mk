$(o)/%/3p/comrak/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/comrak/.extracted: private .INTERNET = 1
$(o)/%/3p/comrak/.extracted: private .CPU = 120
$(o)/%/3p/comrak/.extracted: 3p/comrak/version.lua $(fetch)
	@mkdir -p $(dir $@)
	$(lua_bin) $(fetch) $< comrak $* $(dir $@)
	touch $@

comrak_binaries := $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/comrak/.extracted)
