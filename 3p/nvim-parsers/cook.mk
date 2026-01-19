modules += nvim-parsers
nvim-parsers_srcs := $(wildcard 3p/nvim-parsers/*.tl)
nvim-parsers_deps := nvim nvim-treesitter tree-sitter
nvim-parsers_install := $(o)/3p/nvim-parsers/install.lua

# Parsers are compiled .so files - platform specific, no version.lua
# Built by running nvim with nvim-treesitter to compile parsers
nvim-parsers_out := $(o)/nvim-parsers
nvim-parsers_parsers := $(nvim-parsers_out)/.parsers
nvim-parsers_dir := $(nvim-parsers_parsers)
nvim-parsers_config := $(o)/.config/nvim/parsers.lua

$(nvim-parsers_parsers): $$(nvim_staged) $$(nvim-treesitter_staged) $$(tree-sitter_staged) $(nvim-parsers_config) $(nvim-parsers_install) $(cosmic_bin)
	@rm -rf $(nvim-parsers_out)
	@mkdir -p $(nvim-parsers_out)
	@$(cosmic_bin) $(nvim-parsers_install) $(nvim_staged) $(nvim-treesitter_staged) $(nvim-parsers_out) $(nvim-parsers_config) $(tree-sitter_staged) $@
	@touch $@
