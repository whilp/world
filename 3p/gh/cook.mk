$(o)/%/3p/gh/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/gh/.extracted: private .INTERNET = 1
$(o)/%/3p/gh/.extracted: private .CPU = 120
$(o)/%/3p/gh/.extracted: 3p/gh/version.lua $(fetch)
	@mkdir -p $(dir $@)
	$(lua_bin) $(fetch) $< gh $* $(dir $@)
	touch $@

gh_binaries := $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/gh/.extracted)
