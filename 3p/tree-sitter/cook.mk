$(o)/%/3p/tree-sitter/bin/tree-sitter: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/tree-sitter/bin/tree-sitter: private .INTERNET = 1
$(o)/%/3p/tree-sitter/bin/tree-sitter: private .CPU = 120
$(o)/%/3p/tree-sitter/bin/tree-sitter: 3p/tree-sitter/version.lua $(fetch)
	$(lua_bin) $(fetch) $< tree-sitter $* $(patsubst %/bin/,%/,$(dir $@))

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/tree-sitter/bin/tree-sitter)
