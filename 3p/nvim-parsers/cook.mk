modules += nvim-parsers
nvim-parsers_deps := nvim nvim-treesitter tree-sitter

# Parsers are compiled .so files - platform specific, no version.lua
# Built by running nvim with nvim-treesitter to compile parsers
nvim-parsers_out := $(o)/nvim-parsers
nvim-parsers_parsers := $(nvim-parsers_out)/.parsers
nvim-parsers_dir := $(nvim-parsers_parsers)
nvim-parsers_config := $(o)/.config/nvim/parsers.lua

$(nvim-parsers_parsers): $$(nvim_staged) $$(nvim-treesitter_staged) $$(tree-sitter_staged) $(nvim-parsers_config)
	@rm -rf $(nvim-parsers_out)
	@mkdir -p $(nvim-parsers_out)
	@count=$$(3p/nvim-parsers/install.lua $(nvim_staged) $(nvim-treesitter_staged) $(nvim-parsers_out) $(nvim-parsers_config) $(tree-sitter_staged)); \
	if [ $$? -eq 0 ]; then \
		echo "✓ BUILD  $@ (nvim-parsers: $$count parsers built)"; \
	else \
		exit 1; \
	fi
	@touch $@
