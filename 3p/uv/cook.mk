$(o)/%/3p/uv/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/uv/.extracted: private .INTERNET = 1
$(o)/%/3p/uv/.extracted: private .CPU = 120
$(o)/%/3p/uv/.extracted: 3p/uv/version.lua $(fetch)
	$(lua_bin) $(fetch) $< uv $* $(dir $@)

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/uv/.extracted)
