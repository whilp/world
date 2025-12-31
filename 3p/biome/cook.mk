$(o)/%/3p/biome/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/biome/.extracted: private .INTERNET = 1
$(o)/%/3p/biome/.extracted: private .CPU = 120
$(o)/%/3p/biome/.extracted: 3p/biome/version.lua $(fetch)
	@mkdir -p $(dir $@)
	$(lua_bin) $(fetch) $< biome $* $(dir $@)
	touch $@

biome_binaries := $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/biome/.extracted)
