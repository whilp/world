modules += nvim
nvim_version := 3p/nvim/version.lua
nvim_deps := nvim-conform nvim-mini nvim-lspconfig nvim-treesitter nvim-parsers

# Bundle output location (used by nvim-bundle CI target)
nvim_out := $(o)/bundled/nvim
nvim_pack_dir := $(nvim_out)/share/nvim/site/pack/core/opt

# nvim-bundle: build the bundle locally for CI release workflow
.PHONY: nvim-bundle
nvim-bundle: $(o)/nvim/.bundle

$(o)/nvim/.bundle: $$(nvim_staged) $$(nvim-conform_staged) $$(nvim-mini_staged) $$(nvim-lspconfig_staged) $$(nvim-treesitter_staged) $$(nvim-parsers_parsers)
	@rm -rf $(nvim_out) $@
	@mkdir -p $(dir $(nvim_out))
	@cp -rL $(nvim_staged) $(nvim_out)
	@mkdir -p $(nvim_pack_dir)
	@cp -rL $(nvim-conform_staged) $(nvim_pack_dir)/conform.nvim
	@cp -rL $(nvim-mini_staged) $(nvim_pack_dir)/mini.nvim
	@cp -rL $(nvim-lspconfig_staged) $(nvim_pack_dir)/nvim-lspconfig
	@cp -rL $(nvim-treesitter_staged) $(nvim_pack_dir)/nvim-treesitter
	@cp -r $(o)/nvim-parsers/parser $(nvim_out)/share/nvim/site/
	@echo "generating helptags"
	@$(nvim_out)/bin/nvim --headless "+helptags ALL" "+qa" 2>/dev/null || echo "  skipped"
	@touch $@
