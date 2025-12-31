$(o)/%/3p/sqruff/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/sqruff/.extracted: private .INTERNET = 1
$(o)/%/3p/sqruff/.extracted: private .CPU = 120
$(o)/%/3p/sqruff/.extracted: 3p/sqruff/version.lua $(fetch)
	@mkdir -p $(dir $@)
	$(lua_bin) $(fetch) $< sqruff $* $(dir $@)
	touch $@

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/sqruff/.extracted)
