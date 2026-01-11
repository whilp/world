modules += home
home_srcs := $(wildcard lib/home/*.lua) $(wildcard lib/home/*/*.lua)
home_lib_srcs := $(filter-out lib/home/test%.lua,$(wildcard lib/home/*.lua))
home_libs := $(addprefix $(o)/,$(home_lib_srcs))
home_bin := $(o)/bin/home
# only include home_bin in home_files; home_libs are explicit prereqs of home_bin
# but shouldn't inherit home_deps (which would require staging 20+ tools for linting)
home_files := $(home_bin)
home_tests := lib/home/test_main.lua lib/home/test_versioned.lua
home_release_test := lib/home/test_release.lua

# 3p tools to bundle (nvim handled specially for bundled version)
home_3p_tools := ast-grep biome comrak delta duckdb gh luacheck marksman rg ruff shfmt sqruff stylua superhtml tree-sitter uv

home_deps := cosmos cosmic nvim $(home_3p_tools)

# Build configuration
home_exclude_pattern := ^(3p/|o/|results/|Makefile|\.git)
home_setup_dir := lib/home/setup
home_mac_dir := lib/home/mac
HOME_VERSION ?= $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")

home_built := $(o)/home/.built

# Which nvim to bundle: raw binary for dev, full bundle for release
HOME_NVIM_DIR ?= $(nvim_staged)

$(o)/home/dotfiles.zip: $$(cosmos_staged)
	@mkdir -p $(@D)
	@git ls-files -z | grep -zZvE '$(home_exclude_pattern)' | xargs -0 $(cosmos_zip) -q $@

# Home binary bundles: dotfiles, cosmos binaries, cosmic, 3p tools, lua libs
# Dev build uses raw nvim; release build uses bundled nvim (set via HOME_NVIM_DIR)
$(home_bin): $(home_libs) $(o)/home/dotfiles.zip $$(cosmos_staged) $(cosmic_bin) $$(nvim_staged) $$(foreach t,$(home_3p_tools),$$($$(t)_staged))
	@rm -rf $(home_built)
	@mkdir -p $(home_built)/home/.local/bin $(home_built)/home/.local/share $(home_built)/.lua $(@D)
	@cd $(home_built) && unzip -q $(CURDIR)/$(o)/home/dotfiles.zip -d home
	@$(cp) $(cosmos_dir)/lua $(home_built)/home/.local/bin/lua
	@$(cp) $(cosmos_dir)/unzip $(home_built)/home/.local/bin/unzip
	@$(cp) $(cosmic_bin) $(home_built)/home/.local/bin/cosmic-lua
	@for tool in $(home_3p_tools); do \
		versioned_dir=$$(readlink -f $(o)/$$tool/.staged); \
		versioned_name=$$(basename $$versioned_dir); \
		mkdir -p $(home_built)/home/.local/share/$$tool && \
		cp -r $$versioned_dir $(home_built)/home/.local/share/$$tool/$$versioned_name; \
	done
	@nvim_versioned_name=$$(basename $$(readlink -f $(nvim_staged))); \
		mkdir -p $(home_built)/home/.local/share/nvim && \
		cp -rL $(HOME_NVIM_DIR) $(home_built)/home/.local/share/nvim/$$nvim_versioned_name
	@$(cosmic_bin) lib/home/gen-manifest.lua $(home_built)/home $(HOME_VERSION) > $(home_built)/manifest.lua
	@$(cp) $(cosmos_dir)/lua $@
	@chmod +x $@
	@cd $(home_built) && find home manifest.lua -type f | $(CURDIR)/$(cosmos_zip) -q $(CURDIR)/$@ -@
	@$(cosmos_zip) -qj $@ lib/home/main.lua lib/home/.args
	@cp -r lib/cosmic lib/version.lua lib/claude $(home_setup_dir) $(home_mac_dir) $(home_built)/.lua/
	@cd $(home_built) && $(CURDIR)/$(cosmos_zip) -qr $(CURDIR)/$@ .lua
	@rm -rf $(home_built)

home: $(home_bin)

.PHONY: home

# home-release: rebuild home with nvim bundle (used by test-release)
.PHONY: home-release
home-release: $(nvim_bundle)
	@rm -f $(home_bin)
	@$(MAKE) $(home_bin) HOME_NVIM_DIR=$(nvim_bundle_out)

# Release tests: platform metadata and nvim bundle tests (nvim tests defined in 3p/nvim/cook.mk)
.PHONY: test-release
test-release: home-release nvim-release-tests $(o)/$(home_release_test).tested

# Depend on home-release (not home_bin directly) to avoid parallel build race
$(o)/$(home_release_test).tested: home-release $(home_release_test) $(cosmic_bin) $$(cosmos_staged) | $(bootstrap_files)
	@TEST_RELEASE=1 TEST_DIR=$(home_bin) $(home_release_test) $@
