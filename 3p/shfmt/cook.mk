$(o)/%/3p/shfmt/bin/shfmt: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/shfmt/bin/shfmt: private .INTERNET = 1
$(o)/%/3p/shfmt/bin/shfmt: private .CPU = 120
$(o)/%/3p/shfmt/bin/shfmt: 3p/shfmt/version.lua $(fetch)
	$(lua_bin) $(fetch) $< shfmt $* $(patsubst %/bin/,%/,$(dir $@))

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/shfmt/bin/shfmt)
