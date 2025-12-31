$(o)/%/3p/ruff/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/ruff/.extracted: private .INTERNET = 1
$(o)/%/3p/ruff/.extracted: private .CPU = 120
$(o)/%/3p/ruff/.extracted: 3p/ruff/version.lua $(fetch)
	@mkdir -p $(dir $@)
	$(lua_bin) $(fetch) $< ruff $* $(dir $@)
	touch $@

all_binaries += $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/ruff/.extracted)
