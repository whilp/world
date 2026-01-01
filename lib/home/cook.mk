lib_lua_modules += home
lib_dirs += o/any/home/lib
home_src := $(filter-out lib/home/test%.lua,$(wildcard lib/home/*.lua))
home_lib := $(patsubst lib/%,o/any/home/lib/%,$(home_src))
lib_libs += $(home_lib)
lib_tests += o/any/home/test_main.ok
bins += o/%/home/bin/home

o/any/home/lib/home/%.lua: lib/home/%.lua
	mkdir -p $(@D)
	cp $< $@

o/any/home/test_main.ok: lib/home/test_main.lua $(home_lib) $(runner)
	$(runner) $< $@

# home binary build
home_exclude_pattern := ^(3p/|o/|results/|Makefile|\.git)
home_setup_dir := lib/home/setup
home_mac_dir := lib/home/mac
HOME_VERSION ?= $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")

# 3p tools to bundle in home
home_3p_tools := ast-grep biome comrak delta duckdb gh luacheck marksman nvim rg ruff shfmt sqruff stylua superhtml tree-sitter uv

o/any/home/dotfiles.zip: o/$(current_platform)/cosmos/bin/zip
	mkdir -p $(@D)
	git ls-files -z | grep -zZvE '$(home_exclude_pattern)' | xargs -0 $< -q $@

o/%/home/bin/home: o/%/cosmos/bin/lua o/%/cosmos/bin/zip o/%/cosmos/bin/unzip o/any/home/dotfiles.zip $(lib_libs) lib/home/main.lua lib/home/.args lib/home/gen-manifest.lua $(foreach t,$(home_3p_tools),o/%/$(t)/bin/$(t))
	@echo "Building home binary for $*..."
	rm -rf o/$*/home/staging
	mkdir -p o/$*/home/staging/home/.local/bin o/$*/home/staging/.lua $(@D)
	o/$*/cosmos/bin/unzip -q o/any/home/dotfiles.zip -d o/$*/home/staging/home
	cp -p o/$*/cosmos/bin/lua o/$*/home/staging/home/.local/bin/lua
	cp -p o/$*/cosmos/bin/unzip o/$*/home/staging/home/.local/bin/unzip
	@for tool in $(home_3p_tools); do \
		mkdir -p o/$*/home/staging/home/.local/share/$$tool && \
		cp -r o/$*/$$tool/*-*/ o/$*/home/staging/home/.local/share/$$tool/; \
	done
	$(lua_bin) lib/home/gen-manifest.lua o/$*/home/staging/home $(HOME_VERSION) > o/$*/home/staging/manifest.lua
	cp o/$*/cosmos/bin/lua $@
	cd o/$*/home/staging && find home manifest.lua -type f | $(CURDIR)/o/$*/cosmos/bin/zip -q $(CURDIR)/$@ -@
	o/$*/cosmos/bin/zip -qj $@ lib/home/main.lua lib/home/.args
	cp -r lib/spawn lib/version.lua lib/claude $(home_setup_dir) $(home_mac_dir) o/$*/home/staging/.lua/
	cd o/$*/home/staging && $(CURDIR)/o/$*/cosmos/bin/zip -qr $(CURDIR)/$@ .lua
	rm -rf o/$*/home/staging
	@echo "Built $@"

home: o/$(current_platform)/home/bin/home

home-all: $(foreach p,$(platforms),o/$(p)/home/bin/home) ## Build home for all platforms

.PHONY: home home-all
