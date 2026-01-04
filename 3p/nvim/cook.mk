modules += nvim
nvim_version := 3p/nvim/version.lua
nvim_tests := 3p/nvim/test_nvim.lua 3p/nvim/test_treesitter.lua

# Plugin handling (runs after staging)
nvim_pack_lock := .config/nvim/nvim-pack-lock.json
nvim_plugins := conform.nvim mini.nvim nvim-lspconfig nvim-treesitter
nvim_fetch_plugin := 3p/nvim/fetch-plugin.lua
nvim_bundle := 3p/nvim/bundle.lua

o/any/nvim/plugins/%: $(nvim_pack_lock) $(nvim_fetch_plugin)
	$(nvim_fetch_plugin) $* $@

nvim_plugin_dirs := $(addprefix o/any/nvim/plugins/,$(nvim_plugins))
.PRECIOUS: $(nvim_plugin_dirs)

o/any/nvim/.plugins-fetched: $(nvim_plugin_dirs)
	mkdir -p $(@D)
	touch $@

# Bundle plugins into staged nvim after staging completes
$(nvim_staged): o/any/nvim/.plugins-fetched
	$(nvim_bundle) $(platform) $(nvim_staged) o/any/nvim/plugins
