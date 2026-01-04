modules += nvim
nvim_version := 3p/nvim/version.lua
nvim_tests := 3p/nvim/test_nvim.lua 3p/nvim/test_treesitter.lua
nvim_deps := nvim-conform nvim-mini nvim-lspconfig nvim-treesitter nvim-parsers

nvim_pack_dir = $(o)/bundled/nvim/share/nvim/site/pack/core/opt

# Override bundled: copy staged nvim + plugins + parsers
$(o)/nvim/.bundled: $$(nvim_staged) $$(nvim-conform_staged) $$(nvim-mini_staged) $$(nvim-lspconfig_staged) $$(nvim-treesitter_staged) $$(nvim-parsers_parsers)
	@rm -rf $(o)/bundled/nvim $@
	@mkdir -p $(o)/bundled
	@cp -rL $(nvim_staged) $(o)/bundled/nvim
	@mkdir -p $(nvim_pack_dir)
	@cp -rL $(nvim-conform_staged) $(nvim_pack_dir)/conform.nvim
	@cp -rL $(nvim-mini_staged) $(nvim_pack_dir)/mini.nvim
	@cp -rL $(nvim-lspconfig_staged) $(nvim_pack_dir)/nvim-lspconfig
	@cp -rL $(nvim-treesitter_staged) $(nvim_pack_dir)/nvim-treesitter
	@cp -r $(o)/nvim-parsers/parser $(o)/bundled/nvim/share/nvim/site/
	@echo "generating helptags"
	@$(o)/bundled/nvim/bin/nvim --headless "+helptags ALL" "+qa" 2>/dev/null || echo "  skipped"
	@ln -sfn ../bundled/nvim $@
