$(o)/%/3p/superhtml/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/superhtml/.extracted: private .INTERNET = 1
$(o)/%/3p/superhtml/.extracted: private .CPU = 120
$(o)/%/3p/superhtml/.extracted: 3p/superhtml/version.lua $(fetch)
	@mkdir -p $(dir $@)
	$(lua_bin) $(fetch) $< superhtml $* $(dir $@)
	touch $@

superhtml_binaries := $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/superhtml/.extracted)
