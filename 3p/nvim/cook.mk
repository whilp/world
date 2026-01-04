modules += nvim
nvim_version := 3p/nvim/version.lua
nvim_tests := 3p/nvim/test_nvim.lua 3p/nvim/test_treesitter.lua

# Plugin handling
nvim_pack_lock := .config/nvim/nvim-pack-lock.json
nvim_plugins := conform.nvim mini.nvim nvim-lspconfig nvim-treesitter
nvim_fetch_plugin := 3p/nvim/fetch-plugin.lua
nvim_bundle := 3p/nvim/bundle.lua

$(o)/nvim/plugins/.%-fetched: $(nvim_pack_lock) $(nvim_fetch_plugin)
	@$(nvim_fetch_plugin) $* $(o)/nvim/plugins/$*
	@touch $@

nvim_plugin_fetched := $(foreach p,$(nvim_plugins),$(o)/nvim/plugins/.$(p)-fetched)

$(o)/nvim/.plugins-fetched: $(nvim_plugin_fetched)
	@touch $@

# Override default bundled: copy staged and add plugins/parsers
nvim_bundled_dir := $(o)/bundled/nvim
$(o)/nvim/.bundled: $(o)/nvim/.staged $(o)/nvim/.plugins-fetched $(nvim_bundle)
	@rm -rf $(nvim_bundled_dir) $@
	@mkdir -p $(o)/bundled
	@cp -rL $(o)/nvim/.staged $(nvim_bundled_dir)
	@$(nvim_bundle) $(platform) $(nvim_bundled_dir) $(o)/nvim/plugins
	@ln -sfn ../bundled/nvim $@
