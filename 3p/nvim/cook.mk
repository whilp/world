$(o)/%/3p/nvim/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/nvim/.extracted: private .INTERNET = 1
$(o)/%/3p/nvim/.extracted: private .CPU = 120
$(o)/%/3p/nvim/.extracted: 3p/nvim/version.lua $(fetch)
	$(lua_bin) $(fetch) $< nvim $* $(dir $@)

nvim-latest:
nvim-latest: private .PLEDGE = stdio rpath wpath cpath inet dns
nvim-latest: private .INTERNET = 1
nvim-latest: $(lua_bin)
	$(lua_bin) 3p/nvim/latest.lua

.PHONY: nvim-latest

nvim_pack_lock := .config/nvim/nvim-pack-lock.json
nvim_plugins_dir := $(o)/any/3p/nvim/plugins

ifneq ($(wildcard $(lua_bin)),)
nvim_plugins := $(shell $(lua_bin) lib/build/list-plugins.lua)
else
nvim_plugins :=
endif

fetch_plugin := lib/build/fetch-plugin.lua
$(nvim_plugins_dir)/%/.fetched: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(nvim_plugins_dir)/%/.fetched: private .INTERNET = 1
$(nvim_plugins_dir)/%/.fetched: private .CPU = 60
$(nvim_plugins_dir)/%/.fetched: $(nvim_pack_lock) $(fetch_plugin)
	$(lua_bin) $(fetch_plugin) $* $(dir $@)
	touch $@

nvim_plugin_targets := $(foreach p,$(nvim_plugins),$(nvim_plugins_dir)/$(p)/.fetched)

nvim_bundle := lib/build/nvim-bundle.lua
$(o)/%/3p/nvim/.bundled: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(o)/%/3p/nvim/.bundled: private .INTERNET = 1
$(o)/%/3p/nvim/.bundled: private .CPU = 180
$(o)/%/3p/nvim/.bundled: $(o)/%/3p/nvim/.extracted $(nvim_bundle) $(nvim_plugin_targets)
	$(lua_bin) $(nvim_bundle) $* $(dir $@) $(nvim_plugins_dir)
	touch $@

nvim_bundled := $(foreach p,$(PLATFORMS),$(o)/$(p)/3p/nvim/.bundled)

all_binaries += $(nvim_bundled)
