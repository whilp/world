$(o)/%/3p/ruff/bin/ruff: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/ruff/bin/ruff: private .INTERNET = 1
$(o)/%/3p/ruff/bin/ruff: private .CPU = 120
$(o)/%/3p/ruff/bin/ruff: 3p/ruff/version.lua $(fetch)
	$(lua_bin) $(fetch) $< ruff $* $(patsubst %/bin/,%/,$(dir $@))

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/ruff/bin/ruff)
