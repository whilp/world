$(o)/%/3p/ast-grep/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/ast-grep/.extracted: private .INTERNET = 1
$(o)/%/3p/ast-grep/.extracted: private .CPU = 120
$(o)/%/3p/ast-grep/.extracted: 3p/ast-grep/version.lua $(fetch)
	@mkdir -p $(dir $@)
	$(lua_bin) $(fetch) $< ast-grep $* $(dir $@)
	touch $@

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/ast-grep/.extracted)
