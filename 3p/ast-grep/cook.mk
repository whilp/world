$(o)/%/3p/ast-grep/bin/sg: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/ast-grep/bin/sg: private .INTERNET = 1
$(o)/%/3p/ast-grep/bin/sg: private .CPU = 120
$(o)/%/3p/ast-grep/bin/sg: 3p/ast-grep/version.lua $(fetch)
	$(lua_bin) $(fetch) $< ast-grep $* $(patsubst %/bin/,%/,$(dir $@))

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/ast-grep/bin/sg)
