$(o)/%/3p/tree-sitter/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/tree-sitter/.extracted: private .INTERNET = 1
$(o)/%/3p/tree-sitter/.extracted: private .CPU = 120
$(o)/%/3p/tree-sitter/.extracted: 3p/tree-sitter/version.lua $(fetch)
	@mkdir -p $(dir $@)
	$(lua_bin) $(fetch) $< tree-sitter $* $(dir $@)
	touch $@

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/tree-sitter/.extracted)
