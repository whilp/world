nvim_pack_lock := .config/nvim/nvim-pack-lock.json
nvim_plugins_dir := $(3p)/nvim/plugins

nvim_plugins := $(shell $(lib_lua) lib/build/list-plugins.lua)

fetch_plugin := lib/build/fetch-plugin.lua
$(nvim_plugins_dir)/%/.fetched: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(nvim_plugins_dir)/%/.fetched: private .INTERNET = 1
$(nvim_plugins_dir)/%/.fetched: private .CPU = 60
$(nvim_plugins_dir)/%/.fetched: $(nvim_pack_lock) $(fetch_plugin)
	$(lib_lua) $(fetch_plugin) $* $(dir $@)
	touch $@

nvim_plugin_targets := $(foreach p,$(nvim_plugins),$(nvim_plugins_dir)/$(p)/.fetched)

nvim_bundle := lib/build/nvim-bundle.lua
$(3p)/nvim/%/.bundled: private .PLEDGE = stdio rpath wpath cpath exec proc
$(3p)/nvim/%/.bundled: private .CPU = 60
$(3p)/nvim/%/.bundled: $(3p)/nvim/%/.extracted $(nvim_bundle) $(nvim_plugin_targets)
	$(lib_lua) $(nvim_bundle) $* $(dir $@) $(nvim_plugins_dir)
	touch $@

nvim_bundled := $(foreach p,$(PLATFORMS),$(3p)/nvim/$(p)/.bundled)
