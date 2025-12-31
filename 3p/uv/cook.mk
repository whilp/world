$(o)/%/3p/uv/bin/uv: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/uv/bin/uv: private .INTERNET = 1
$(o)/%/3p/uv/bin/uv: private .CPU = 120
$(o)/%/3p/uv/bin/uv: 3p/uv/version.lua $(fetch)
	$(lua_bin) $(fetch) $< uv $* $(patsubst %/bin/,%/,$(dir $@))

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/uv/bin/uv)
