modules += home
home_srcs := $(wildcard lib/home/*.lua) $(wildcard lib/home/*.tl) $(wildcard lib/home/*/*.lua) $(wildcard lib/home/*/*.tl)
home_lib_srcs := $(filter-out lib/home/test%.lua,$(wildcard lib/home/*.lua))
home_libs := $(addprefix $(o)/,$(home_lib_srcs))
home_bin := $(o)/bin/home
# only include home_bin in home_files; home_libs are explicit prereqs of home_bin
# but shouldn't inherit home_deps (which would require staging 20+ tools for linting)
home_files := $(home_bin)
home_tests := lib/home/test_main.tl lib/home/test_versioned.tl
home_tl_files := lib/home/main.tl lib/home/gen-manifest.tl $(wildcard lib/home/setup/*.tl) $(wildcard lib/home/mac/*.tl)

# 3p tools to bundle (nvim handled specially for bundled version)
home_3p_tools := ast-grep biome comrak delta duckdb gh marksman rg ruff shfmt sqruff stylua superhtml tree-sitter uv

home_deps := cosmos cosmic nvim $(home_3p_tools)

# Build configuration
home_exclude_pattern := ^(3p/|o/|results/|Makefile|\.git)
home_setup_dir := lib/home/setup
home_mac_dir := lib/home/mac
HOME_VERSION ?= $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")

home_built := $(o)/home/.built

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

$(o)/home/dotfiles.zip: $$(cosmos_staged) $(cosmic_bin)
	@rm -rf $(o)/home/.dotfiles-staging
	@mkdir -p $(@D) $(o)/home/.dotfiles-staging
	@git archive --format=tar HEAD | tar -x -C $(o)/home/.dotfiles-staging
	@find $(o)/home/.dotfiles-staging -type f -o -type d | grep -E '$(home_exclude_pattern)' | xargs rm -rf 2>/dev/null || true
	@mkdir -p $(o)/home/.dotfiles-staging/.local/bin
	@$(cp) $(cosmic_bin) $(o)/home/.dotfiles-staging/.local/bin/cosmic-lua
	@ln -sf cosmic-lua $(o)/home/.dotfiles-staging/.local/bin/lua
	@cd $(o)/home/.dotfiles-staging && $(CURDIR)/$(cosmos_zip) -qry $(CURDIR)/$@ .

# Compiled .lua from home_tl_files (Makefile compiles these automatically)
home_tl_lua := $(patsubst %.tl,$(o)/%.lua,$(home_tl_files))

# Home binary bundles: dotfiles.zip, cosmos binaries, 3p tools, lua libs
# Dev build uses raw nvim; release build uses bundled nvim (set via HOME_NVIM_DIR)
$(home_bin): $(home_libs) $(home_tl_lua) $(home_nvim_tl_compiled) $(o)/home/dotfiles.zip $$(cosmos_staged) $(cosmic_bin) $(cosmic_tl_libs) $$(nvim_staged) $$(foreach t,$(home_3p_tools),$$($$(t)_staged))
	@rm -rf $(home_built)
	@mkdir -p $(home_built)/home/.local/bin $(home_built)/home/.local/share $(home_built)/.lua $(@D)
	@cp $(o)/home/dotfiles.zip $(home_built)/dotfiles.zip
	@if [ -n "$(home_nvim_tl_compiled)" ]; then \
		mkdir -p $(home_built)/nvim-tl-compiled; \
		for f in $(home_nvim_tl_compiled); do \
			target=$${f#$(o)/}; \
			mkdir -p $(home_built)/nvim-tl-compiled/$$(dirname $$target); \
			cp $$f $(home_built)/nvim-tl-compiled/$$target; \
		done; \
	fi
	@$(cp) $(cosmos_dir)/unzip $(home_built)/home/.local/bin/unzip
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
	@$(cosmos_zip) -qj $@ $(home_built)/dotfiles.zip $(home_built)/manifest.lua $(o)/lib/home/main.lua lib/home/.args
	@if [ -n "$(home_nvim_tl_compiled)" ]; then \
		cd $(home_built)/nvim-tl-compiled && find . -type f | $(CURDIR)/$(cosmos_zip) -q $(CURDIR)/$@ -@; \
	fi
	@cd $(home_built) && find home -type f -o -type l | $(CURDIR)/$(cosmos_zip) -qy $(CURDIR)/$@ -@
	@cp -r lib/cosmic lib/version.lua lib/claude $(home_built)/.lua/
	@mkdir -p $(home_built)/.lua/setup $(home_built)/.lua/mac
	@cp $(o)/$(home_setup_dir)/*.lua $(home_built)/.lua/setup/
	@cp $(o)/$(home_mac_dir)/*.lua $(home_built)/.lua/mac/
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

# Release tests: nvim bundle tests (nvim tests defined in 3p/nvim/cook.mk)
.PHONY: test-release
test-release: home-release nvim-release-tests
