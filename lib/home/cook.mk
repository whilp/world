modules += home
home_srcs := $(wildcard lib/home/*.lua) $(wildcard lib/home/*.tl) $(wildcard lib/home/*/*.lua) $(wildcard lib/home/*/*.tl)
home_lib_srcs := $(filter-out lib/home/test%.lua,$(wildcard lib/home/*.lua))
home_libs := $(addprefix $(o)/,$(home_lib_srcs))
home_bin := $(o)/bin/home
# only include home_bin in home_files; home_libs are explicit prereqs of home_bin
# but shouldn't inherit home_deps (which would require staging 20+ tools for linting)
home_files := $(home_bin)
home_tests := lib/home/test_main.tl lib/home/test_versioned.tl
home_release_test := lib/home/test_release.tl

# 3p tools to bundle (nvim handled specially for bundled version)
home_3p_tools := ast-grep biome comrak delta duckdb gh marksman rg ruff shfmt sqruff stylua superhtml tree-sitter uv

home_deps := cosmos cosmic nvim clasp $(home_3p_tools)

# Build configuration
home_exclude_pattern := ^(3p/|o/|results/|Makefile|\.git)
home_setup_dir := lib/home/setup
home_mac_dir := lib/home/mac
HOME_VERSION ?= $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")

home_built := $(o)/home/.built

# Teal-compiled scripts
home_tl_compiled := $(o)/lib/home/main.lua $(o)/lib/home/gen-manifest.lua $(o)/lib/home/gen-platforms.lua

# Nvim config teal files (compiled at build time, shipped as .lua)
home_nvim_tl_srcs := $(shell find .config/nvim -name '*.tl' 2>/dev/null)
home_nvim_tl_compiled := $(patsubst %.tl,$(o)/%.lua,$(home_nvim_tl_srcs))

# Compile nvim .tl configs to .lua
# Uses secondary expansion so $(tl_files) is evaluated after all includes
.SECONDEXPANSION:
$(o)/.config/nvim/%.lua: .config/nvim/%.tl $$(tl_files) $$(bootstrap_files) | $$(tl_staged)
	@mkdir -p $(@D)
	@$(tl_gen) $< -o $@

# Which nvim to bundle: raw binary for dev, full bundle for release
HOME_NVIM_DIR ?= $(nvim_staged)

$(o)/home/dotfiles.zip: $$(cosmos_staged)
	@mkdir -p $(@D)
	@git ls-files -z | grep -zZvE '$(home_exclude_pattern)' | xargs -0 $(cosmos_zip) -q $@

# Home binary bundles: dotfiles, cosmos binaries, cosmic, 3p tools, lua libs
# Dev build uses raw nvim; release build uses bundled nvim (set via HOME_NVIM_DIR)
$(home_bin): $(home_libs) $(home_tl_compiled) $(home_nvim_tl_compiled) $(o)/home/dotfiles.zip $$(cosmos_staged) $(cosmic_bin) $(clasp_bin) $(cosmic_tl_libs) $$(nvim_staged) $$(foreach t,$(home_3p_tools),$$($$(t)_staged))
	@rm -rf $(home_built)
	@mkdir -p $(home_built)/home/.local/bin $(home_built)/home/.local/share $(home_built)/.lua $(@D)
	@cd $(home_built) && unzip -q $(CURDIR)/$(o)/home/dotfiles.zip -d home
	@if [ -n "$(home_nvim_tl_compiled)" ]; then \
		for f in $(home_nvim_tl_compiled); do \
			target=$${f#$(o)/}; \
			mkdir -p $(home_built)/home/$$(dirname $$target); \
			cp $$f $(home_built)/home/$$target; \
		done; \
		find $(home_built)/home/.config/nvim -name '*.tl' -delete 2>/dev/null || true; \
	fi
	@$(cp) $(cosmos_dir)/lua $(home_built)/home/.local/bin/lua
	@$(cp) $(cosmos_dir)/unzip $(home_built)/home/.local/bin/unzip
	@$(cp) $(cosmic_bin) $(home_built)/home/.local/bin/cosmic-lua
	@$(cp) $(clasp_bin) $(home_built)/home/.local/bin/clasp
	@for tool in $(home_3p_tools); do \
		versioned_dir=$$(readlink -f $(o)/$$tool/.staged); \
		versioned_name=$$(basename $$versioned_dir); \
		mkdir -p $(home_built)/home/.local/share/$$tool && \
		cp -r $$versioned_dir $(home_built)/home/.local/share/$$tool/$$versioned_name; \
	done
	@nvim_versioned_name=$$(basename $$(readlink -f $(nvim_staged))); \
		mkdir -p $(home_built)/home/.local/share/nvim && \
		cp -rL $(HOME_NVIM_DIR) $(home_built)/home/.local/share/nvim/$$nvim_versioned_name
	@$(cosmic_bin) $(o)/lib/home/gen-manifest.lua $(home_built)/home $(HOME_VERSION) > $(home_built)/manifest.lua
	@$(cp) $(cosmos_dir)/lua $@
	@chmod +x $@
	@cd $(home_built) && find home manifest.lua -type f | $(CURDIR)/$(cosmos_zip) -q $(CURDIR)/$@ -@
	@$(cosmos_zip) -qj $@ $(o)/lib/home/main.lua lib/home/.args
	@cp -r lib/cosmic lib/version.lua lib/claude $(home_setup_dir) $(home_mac_dir) $(home_built)/.lua/
	@cp -f $(cosmic_tl_libs) $(home_built)/.lua/cosmic/
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
# Run compiled .lua (derived from home_release_test path)
home_release_test_lua := $(o)/$(basename $(home_release_test)).lua
$(o)/$(home_release_test).tested: home-release $(home_release_test_lua) $(cosmic_bin) $$(cosmos_staged) | $(bootstrap_files)
	@[ -x $(home_release_test_lua) ] || chmod a+x $(home_release_test_lua)
	@TEST_RELEASE=1 TEST_DIR=$(home_bin) $(test_runner) $(home_release_test_lua) > $@
