modules += home
home_srcs := $(wildcard lib/home/*.lua) $(wildcard lib/home/*.tl) $(wildcard lib/home/*/*.lua) $(wildcard lib/home/*/*.tl)
home_lib_srcs := $(filter-out lib/home/test%.lua,$(wildcard lib/home/*.lua))
home_libs := $(addprefix $(o)/,$(home_lib_srcs))
home_bin := $(o)/bin/home
# only include home_bin in home_files; home_libs are explicit prereqs of home_bin
# but shouldn't inherit home_deps (which would require staging 20+ tools for linting)
home_files := $(home_bin)
home_tests := $(wildcard lib/home/test_*.tl)
home_tl_files := lib/home/main.tl lib/home/gen-manifest.tl lib/home/bootstrap.tl $(wildcard lib/home/setup/*.tl) $(wildcard lib/home/mac/*.tl)

# 3p tools to bundle (nvim handled specially for bundled version)
home_3p_tools := ast-grep biome bun comrak delta duckdb gh marksman rg ruff shfmt sqruff stylua superhtml tree-sitter uv

home_deps := cosmos cosmic nvim clasp $(home_3p_tools)

# Build configuration
home_setup_dir := lib/home/setup
home_mac_dir := lib/home/mac
HOME_VERSION ?= $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")

# Dotfiles sources (explicit wildcards for Make dependency tracking)
# Shell configs
dots_shell := .zshrc .zshenv .zprofile
# Editor/linter configs
dots_editor := .editorconfig .stylua.toml .luacheckrc tlconfig.lua biome.json sgconfig.yml
# Tool configs
dots_tools := .aerospace.toml .watchmanconfig
# Claude config
dots_claude := .claude/.ignore .claude/settings.json \
    $(wildcard .claude/*.md) $(wildcard .claude/commands/*.md) \
    $(wildcard .claude/skills/*/*.md)
# AST-grep rules
dots_ast_grep := $(wildcard .ast-grep/rules/*.yml)
# Hammerspoon symlink (files are in .config/hammerspoon via dots_config)
dots_hammerspoon := .hammerspoon
# .config/* (flat subdirs)
dots_config := $(wildcard .config/delta/*) $(wildcard .config/fish/*) \
    $(wildcard .config/gh/*) $(wildcard .config/ghostty/*) \
    $(wildcard .config/git/*) $(wildcard .config/karabiner/*) \
    $(wildcard .config/ripgrep/*) $(wildcard .config/ssh/*) \
    $(wildcard .config/voyager/*) $(wildcard .config/hammerspoon/*)
# .config/nvim (nested structure)
# Source .tl files are compiled to .lua at build time; only ship .lua
dots_nvim_tl := $(wildcard .config/nvim/*.tl) $(wildcard .config/nvim/plugin/*.tl)
dots_nvim := $(wildcard .config/nvim/*.lua) $(wildcard .config/nvim/plugin/*.lua) \
    $(wildcard .config/nvim/queries/*/*.scm)
# .local/bin scripts
dots_local_bin := $(wildcard .local/bin/*)
# Root files
dots_root := CLAUDE.md LICENSE README.md bootstrap.mk cook.mk setup.sh
# bin/ scripts
dots_bin := $(wildcard bin/*)
# lib/ sources (comprehensive wildcards)
dots_lib := lib/cook.mk $(wildcard lib/*.lua) $(wildcard lib/*.tl) \
    $(wildcard lib/*/cook.mk) $(wildcard lib/*/*.lua) $(wildcard lib/*/*.tl) \
    $(wildcard lib/*/*.js) $(wildcard lib/*/*.json) \
    $(wildcard lib/*/*.snap) $(wildcard lib/*/*/*.lua) $(wildcard lib/*/*/*.tl) \
    $(wildcard lib/types/*.d.tl) $(wildcard lib/types/*/*.d.tl) \
    $(wildcard lib/*/.args) $(wildcard lib/*/.claspignore) $(wildcard lib/*/MANIFEST.txt)

home_built := $(o)/home/.built

# Nvim config teal files (compiled at build time, shipped as .lua)
home_nvim_tl_compiled := $(patsubst %.tl,$(o)/%.lua,$(dots_nvim_tl))

# Aggregate all dotfiles
home_dotfiles := $(dots_shell) $(dots_editor) $(dots_tools) $(dots_claude) \
    $(dots_ast_grep) $(dots_hammerspoon) $(dots_config) $(dots_nvim) \
    $(dots_local_bin) $(dots_root) $(dots_bin) $(dots_lib)

# Compile nvim .tl configs to .lua
# Uses secondary expansion so $(tl_files) is evaluated after all includes
.SECONDEXPANSION:
$(o)/.config/nvim/%.lua: .config/nvim/%.tl $$(tl_files) $$(bootstrap_files) | $$(tl_staged)
	@mkdir -p $(@D)
	@$(tl_gen) $< -o $@

# Always use bundled nvim (with plugins)
# Override generic zip rule for nvim to use bundle instead of raw staged
$(o)/nvim/.zip: $(nvim_bundle) $$(cosmos_staged)
	@rm -rf $(@D)/.zip-staging
	@mkdir -p $(@D)/.zip-staging/.local/share/nvim
	@versioned_name=$$(basename $$(readlink -f $(nvim_staged)))-bundled && \
		cp -r $(nvim_bundle_out) $(@D)/.zip-staging/.local/share/nvim/$$versioned_name && \
		for item in $(@D)/.zip-staging/.local/share/nvim/$$versioned_name/*; do \
			ln -sf $$versioned_name/$$(basename $$item) $(@D)/.zip-staging/.local/share/nvim/$$(basename $$item); \
		done && \
		cd $(@D)/.zip-staging && $(CURDIR)/$(cosmos_zip) -qry $(CURDIR)/$@ .
	@rm -rf $(@D)/.zip-staging

# Create dotfiles.zip with symlinks preserved
# Includes: dotfiles, compiled nvim configs, cosmic-lua binary, clasp binary, lua symlink
$(o)/home/dotfiles.zip: $(home_dotfiles) $$(cosmos_staged) $(cosmic_bin) $$(clasp_bin) $(home_nvim_tl_compiled)
	@rm -rf $(o)/home/.dotfiles-staging
	@mkdir -p $(@D) $(o)/home/.dotfiles-staging
	@for f in $(home_dotfiles); do \
		mkdir -p $(o)/home/.dotfiles-staging/$$(dirname "$$f") && \
		cp -a "$$f" $(o)/home/.dotfiles-staging/"$$f"; \
	done
	@for f in $(home_nvim_tl_compiled); do \
		target=$${f#$(o)/}; \
		mkdir -p $(o)/home/.dotfiles-staging/$$(dirname $$target); \
		cp $$f $(o)/home/.dotfiles-staging/$$target; \
	done
	@mkdir -p $(o)/home/.dotfiles-staging/.local/bin
	@$(cp) $(cosmic_bin) $(o)/home/.dotfiles-staging/.local/bin/cosmic-lua
	@$(cp) $(clasp_bin) $(o)/home/.dotfiles-staging/.local/bin/clasp
	@ln -sf cosmic-lua $(o)/home/.dotfiles-staging/.local/bin/lua
	@cd $(o)/home/.dotfiles-staging && $(CURDIR)/$(cosmos_zip) -qry $(CURDIR)/$@ .
	@rm -rf $(o)/home/.dotfiles-staging

# Compiled .lua from home_tl_files (Makefile compiles these automatically)
home_tl_lua := $(patsubst %.tl,$(o)/%.lua,$(home_tl_files))

# Home binary bundles: dotfiles.zip, per-tool zips (extracted at runtime), lua libs
# Tool zips use secondary expansion to defer $(x_zip) evaluation
$(home_bin): $(home_libs) $(home_tl_lua) $(o)/home/dotfiles.zip $$(cosmos_staged) $(cosmic_bin) $(cosmic_tl_libs) $$(foreach t,$(home_3p_tools) nvim,$$($$(t)_zip))
	@rm -rf $(home_built)
	@mkdir -p $(home_built)/tools $(home_built)/.lua $(@D)
	@$(cp) $(o)/home/dotfiles.zip $(home_built)/dotfiles.zip
	@$(cp) $(cosmos_dir)/unzip $(home_built)/unzip
	@for tool in $(home_3p_tools) nvim; do \
		$(cp) $(o)/$$tool/.zip $(home_built)/tools/$$tool.zip; \
	done
	@echo 'return { version = "$(HOME_VERSION)", tools = { $(foreach t,$(home_3p_tools) nvim,"$(t)", ) } }' > $(home_built)/manifest.lua
	@$(cp) $(cosmos_dir)/lua $@
	@chmod +x $@
	@cd $(home_built) && find tools unzip dotfiles.zip manifest.lua -type f | $(CURDIR)/$(cosmos_zip) -qy $(CURDIR)/$@ -@
	@$(cosmos_zip) -qj $@ $(o)/lib/home/main.lua lib/home/.args
	@cp -r lib/cosmic lib/version.lua lib/claude $(home_built)/.lua/
	@mkdir -p $(home_built)/.lua/setup $(home_built)/.lua/mac
	@cp $(o)/$(home_setup_dir)/*.lua $(home_built)/.lua/setup/
	@cp $(o)/$(home_mac_dir)/*.lua $(home_built)/.lua/mac/
	@cp -f $(cosmic_tl_libs) $(home_built)/.lua/cosmic/
	@cd $(home_built) && $(CURDIR)/$(cosmos_zip) -qr $(CURDIR)/$@ .lua
	@rm -rf $(home_built)

home: $(home_bin)

.PHONY: home

# Bootstrap binary: lightweight binary for sprite bootstrap
bootstrap_bin := $(o)/bin/bootstrap
bootstrap_built := $(o)/bootstrap/.built
bootstrap_main := $(o)/lib/home/bootstrap.lua
bootstrap_spawn := $(o)/lib/cosmic/spawn.lua

$(bootstrap_bin): $(bootstrap_main) $(bootstrap_spawn) $$(cosmos_staged)
	@rm -rf $(bootstrap_built)
	@mkdir -p $(bootstrap_built)/.lua/cosmic $(@D)
	@$(cp) $(bootstrap_spawn) $(bootstrap_built)/.lua/cosmic/
	@$(cp) $(cosmos_lua) $@
	@chmod +x $@
	@cd $(bootstrap_built) && $(CURDIR)/$(cosmos_zip) -qr $(CURDIR)/$@ .lua
	@$(cosmos_zip) -qj $@ $(bootstrap_main)
	@echo '/zip/bootstrap.lua' > $(bootstrap_built)/.args
	@cd $(bootstrap_built) && $(CURDIR)/$(cosmos_zip) -qj $(CURDIR)/$@ .args
	@rm -rf $(bootstrap_built)

bootstrap: $(bootstrap_bin)

.PHONY: bootstrap

# bootstrap test depends on the bootstrap binary
$(o)/lib/home/test_bootstrap.tl.test.ok: $(bootstrap_bin)

# Release tests: nvim bundle tests (nvim tests defined in 3p/nvim/cook.mk)
# Note: home always includes bundled nvim now, so no separate home-release needed
.PHONY: test-release
test-release: $(home_bin) nvim-release-tests
