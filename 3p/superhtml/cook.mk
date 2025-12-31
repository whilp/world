$(o)/%/3p/superhtml/bin/superhtml: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/superhtml/bin/superhtml: private .INTERNET = 1
$(o)/%/3p/superhtml/bin/superhtml: private .CPU = 120
$(o)/%/3p/superhtml/bin/superhtml: 3p/superhtml/version.lua $(fetch)
	$(lua_bin) $(fetch) $< superhtml $* $(patsubst %/bin/,%/,$(dir $@))

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/superhtml/bin/superhtml)
