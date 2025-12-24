nvim_pack_lock := .config/nvim/nvim-pack-lock.json
nvim_bundle := lib/build/nvim-bundle.lua

$(3p)/nvim/%/.bundled: $(3p)/nvim/%/.extracted $(nvim_bundle) $(nvim_pack_lock)
	$(lib_lua) $(nvim_bundle) $* $(dir $@)
	touch $@

nvim_bundled := $(foreach p,$(PLATFORMS),$(3p)/nvim/$(p)/.bundled)
