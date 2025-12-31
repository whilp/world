$(o)/%/3p/marksman/bin/marksman: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/marksman/bin/marksman: private .INTERNET = 1
$(o)/%/3p/marksman/bin/marksman: private .CPU = 120
$(o)/%/3p/marksman/bin/marksman: 3p/marksman/version.lua $(fetch)
	$(lua_bin) $(fetch) $< marksman $* $(patsubst %/bin/,%/,$(dir $@))

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/marksman/bin/marksman)
