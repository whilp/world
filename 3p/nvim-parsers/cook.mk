modules += nvim-parsers
nvim-parsers_deps := nvim nvim-treesitter tree-sitter

# Parsers are compiled .so files - platform specific, no version.lua
# Built by running nvim with nvim-treesitter to compile parsers
nvim-parsers_dir := $(o)/nvim-parsers
nvim-parsers_parsers := $(o)/nvim-parsers/.parsers

$(nvim-parsers_parsers): $$(nvim_staged) $$(nvim-treesitter_staged) $$(tree-sitter_staged) .config/nvim/parsers.lua
	@rm -rf $(nvim-parsers_dir)
	@mkdir -p $(nvim-parsers_dir)
	@3p/nvim-parsers/install.lua $(nvim_staged) $(nvim-treesitter_staged) $(nvim-parsers_dir)
	@touch $@
