$(o)/%/3p/shfmt/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/shfmt/.extracted: private .INTERNET = 1
$(o)/%/3p/shfmt/.extracted: private .CPU = 120
$(o)/%/3p/shfmt/.extracted: 3p/shfmt/version.lua $(fetch)
	@mkdir -p $(dir $@)
	$(lua_bin) $(fetch) $< shfmt $* $(dir $@)
	touch $@

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/shfmt/.extracted)
