$(o)/%/3p/stylua/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/stylua/.extracted: private .INTERNET = 1
$(o)/%/3p/stylua/.extracted: private .CPU = 120
$(o)/%/3p/stylua/.extracted: 3p/stylua/version.lua $(fetch)
	@mkdir -p $(dir $@)
	$(lua_bin) $(fetch) $< stylua $* $(dir $@)
	touch $@

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/stylua/.extracted)
