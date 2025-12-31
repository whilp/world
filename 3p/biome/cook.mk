$(o)/%/3p/biome/bin/biome: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/biome/bin/biome: private .INTERNET = 1
$(o)/%/3p/biome/bin/biome: private .CPU = 120
$(o)/%/3p/biome/bin/biome: 3p/biome/version.lua $(fetch)
	$(lua_bin) $(fetch) $< biome $* $(patsubst %/bin/,%/,$(dir $@))

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/biome/bin/biome)
