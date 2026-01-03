modules += home
home_srcs := $(filter-out lib/home/test%.lua,$(wildcard lib/home/*.lua))
home_libs := $(addprefix $(o)/,$(home_srcs))
home_bin := $(o)/bin/home
home_files := $(home_bin) $(home_libs)
home_tests := lib/home/test_main.lua

# 3p tools to bundle
home_3p_tools := ast-grep biome comrak delta duckdb gh luacheck marksman nvim rg ruff shfmt sqruff stylua superhtml tree-sitter uv

home_deps := cosmos cosmic $(home_3p_tools)

# Copy lib sources to output
$(o)/lib/home/%.lua: lib/home/%.lua
	@mkdir -p $(@D)
	@$(cp) $< $@

# Build configuration
home_exclude_pattern := ^(3p/|o/|results/|Makefile|\.git)
home_setup_dir := lib/home/setup
home_mac_dir := lib/home/mac
HOME_VERSION ?= $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")

home_built := $(o)/home/.built

$(o)/home/dotfiles.zip: $$(cosmos_staged)
	@mkdir -p $(@D)
	git ls-files -z | grep -zZvE '$(home_exclude_pattern)' | xargs -0 $(cosmos_zip) -q $@

# Home binary bundles: dotfiles, cosmos binaries, cosmic, 3p tools, lua libs
$(home_bin): $(home_libs) $(o)/home/dotfiles.zip $$(cosmos_staged) $(cosmic_bin) $$(foreach t,$(home_3p_tools),$$($$(t)_staged))
	@echo "Building home binary..."
	@rm -rf $(home_built)
	@mkdir -p $(home_built)/home/.local/bin $(home_built)/home/.local/share $(home_built)/.lua $(@D)
	@cd $(home_built) && unzip -q $(CURDIR)/$(o)/home/dotfiles.zip -d home
	@$(cp) $(cosmos_dir)/lua $(home_built)/home/.local/bin/lua
	@$(cp) $(cosmos_dir)/unzip $(home_built)/home/.local/bin/unzip
	@$(cp) $(cosmic_bin) $(home_built)/home/.local/bin/cosmic-lua
	@for tool in $(home_3p_tools); do \
		mkdir -p $(home_built)/home/.local/share/$$tool && \
		cp -r $(o)/$$tool/.staged/* $(home_built)/home/.local/share/$$tool/; \
	done
	@$(cosmic_bin) lib/home/gen-manifest.lua $(home_built)/home $(HOME_VERSION) > $(home_built)/manifest.lua
	@$(cp) $(cosmos_dir)/lua $@
	@chmod +x $@
	@cd $(home_built) && find home manifest.lua -type f | $(CURDIR)/$(cosmos_zip) -q $(CURDIR)/$@ -@
	@$(cosmos_zip) -qj $@ lib/home/main.lua lib/home/.args
	@cp -r lib/cosmic lib/version.lua lib/claude $(home_setup_dir) $(home_mac_dir) $(home_built)/.lua/
	@cd $(home_built) && $(CURDIR)/$(cosmos_zip) -qr $(CURDIR)/$@ .lua
	@rm -rf $(home_built)
	@echo "Built $@"

home: $(home_bin)

.PHONY: home
