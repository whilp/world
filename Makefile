include 3p/cook.mk
include 3p/luaunit/cook.mk
include 3p/cosmopolitan/cook.mk
include 3p/make/cook.mk
include 3p/lua/cook.mk
include 3p/nvim/cook.mk
include 3p/gh/cook.mk
include 3p/delta/cook.mk
include 3p/rg/cook.mk
include 3p/duckdb/cook.mk
include 3p/tree-sitter/cook.mk
include 3p/ast-grep/cook.mk
include 3p/biome/cook.mk
include 3p/comrak/cook.mk
include 3p/marksman/cook.mk
include 3p/ruff/cook.mk
include 3p/shfmt/cook.mk
include 3p/sqruff/cook.mk
include 3p/stylua/cook.mk
include 3p/superhtml/cook.mk
include 3p/uv/cook.mk
include src/test.mk

build: lua

clean:
	rm -rf o results

home_exclude_pattern = ^(3p/|o/|results/|Makefile|home/|\.git)

results/dotfiles.zip: private .UNVEIL = \
	r:$(CURDIR) \
	rx:$(cosmos_zip_bin) \
	rwc:results \
	rw:/dev/null
results/dotfiles.zip: $(cosmos_zip_bin) | results
	git ls-files -z | grep -zZvE '$(home_exclude_pattern)' | \
		xargs -0 $(cosmos_zip_bin) -q -r $@

# Aggregate all binary extraction markers
all_binaries := \
	$(nvim_binaries) \
	$(gh_binaries) \
	$(delta_binaries) \
	$(rg_binaries) \
	$(duckdb_binaries) \
	$(tree_sitter_binaries) \
	$(ast_grep_binaries) \
	$(biome_binaries) \
	$(comrak_binaries) \
	$(marksman_binaries) \
	$(ruff_binaries) \
	$(shfmt_binaries) \
	$(sqruff_binaries) \
	$(stylua_binaries) \
	$(superhtml_binaries) \
	$(uv_binaries)

# Platform-specific binaries zips
results/binaries-darwin-arm64.zip: private .UNVEIL = \
	r:$(3p) \
	rx:$(cosmos_zip_bin) \
	rwc:results \
	rw:/dev/null
results/binaries-darwin-arm64.zip: $(all_binaries) $(cosmos_zip_bin) | results
	cd $(3p) && \
		find . -path '*/darwin-arm64/*' -type f ! -name '.extracted' | \
		$(cosmos_zip_bin) -q $(CURDIR)/$@ -@

results/binaries-linux-arm64.zip: private .UNVEIL = \
	r:$(3p) \
	rx:$(cosmos_zip_bin) \
	rwc:results \
	rw:/dev/null
results/binaries-linux-arm64.zip: $(all_binaries) $(cosmos_zip_bin) | results
	cd $(3p) && \
		find . -path '*/linux-arm64/*' -type f ! -name '.extracted' | \
		$(cosmos_zip_bin) -q $(CURDIR)/$@ -@

results/binaries-linux-x86_64.zip: private .UNVEIL = \
	r:$(3p) \
	rx:$(cosmos_zip_bin) \
	rwc:results \
	rw:/dev/null
results/binaries-linux-x86_64.zip: $(all_binaries) $(cosmos_zip_bin) | results
	cd $(3p) && \
		find . -path '*/linux-x86_64/*' -type f ! -name '.extracted' | \
		$(cosmos_zip_bin) -q $(CURDIR)/$@ -@

# Platform-specific home binaries
define build_home
	@echo "Building $(1) for $(2)..."
	@rm -rf results/home-$(2)
	@mkdir -p results/home-$(2)/home
	@echo "Extracting dotfiles..."
	@unzip -q results/dotfiles.zip -d results/home-$(2)/home
	@echo "Extracting and organizing binaries..."
	@mkdir -p results/home-$(2)/temp-binaries
	@unzip -q results/binaries-$(2).zip -d results/home-$(2)/temp-binaries
	@mkdir -p results/home-$(2)/home/.local/bin results/home-$(2)/home/.local/share
	@cd results/home-$(2)/temp-binaries && \
		for tool in nvim gh delta rg duckdb tree-sitter ast-grep biome comrak marksman ruff shfmt sqruff stylua superhtml uv; do \
			if [ -d "$$tool/$(2)" ]; then \
				echo "  Installing $$tool..."; \
				if [ "$$tool" = "nvim" ]; then \
					mkdir -p $(CURDIR)/results/home-$(2)/home/.local/share/nvim; \
					cp -r $$tool/$(2)/* $(CURDIR)/results/home-$(2)/home/.local/share/nvim/; \
				else \
					if [ -d "$$tool/$(2)/bin" ]; then \
						exe=$$(find "$$tool/$(2)/bin" -maxdepth 1 -type f -name "$$tool" 2>/dev/null | head -1); \
						if [ -n "$$exe" ]; then cp -p "$$exe" $(CURDIR)/results/home-$(2)/home/.local/bin/$$tool; fi; \
					else \
						exe=$$(find "$$tool/$(2)" -maxdepth 1 -type f -name "$$tool" 2>/dev/null | head -1); \
						if [ -n "$$exe" ]; then cp -p "$$exe" $(CURDIR)/results/home-$(2)/home/.local/bin/$$tool; fi; \
					fi; \
					for dir in lib share libexec; do \
						if [ -d "$$tool/$(2)/$$dir" ]; then \
							mkdir -p $(CURDIR)/results/home-$(2)/home/.local/share/$$tool; \
							cp -r "$$tool/$(2)/$$dir" $(CURDIR)/results/home-$(2)/home/.local/share/$$tool/; \
						fi; \
					done; \
				fi; \
			fi; \
		done
	@rm -rf results/home-$(2)/temp-binaries
	@cp -p $(lua_bin) results/home-$(2)/home/.local/bin/lua
	@cp -p o/3p/cosmos/bin/unzip results/home-$(2)/home/.local/bin/unzip
	@echo "Generating manifest..."
	@cd results/home-$(2) && find home \( -type f -o -type l \) -exec sh -c 'printf "%s %s\n" "$$1" $$(stat -c "%a" "$$1" 2>/dev/null || stat -f "%Lp" "$$1")' _ {} \; | sort > MANIFEST.txt
	@echo "Creating home binary..."
	@cp $(lua_bin) $(1)
	@cd results/home-$(2) && find . -type f -o -type l | $(cosmos_zip_bin) -q $(CURDIR)/$(1) -@
	@cd home && $(cosmos_zip_bin) -qr $(CURDIR)/$(1) main.lua .args
	@rm -rf results/home-$(2)
endef

results/bin/home-darwin-arm64: $(lua_bin) results/dotfiles.zip results/binaries-darwin-arm64.zip home/main.lua home/.args | results/bin
	$(call build_home,$@,darwin-arm64)

results/bin/home-linux-arm64: $(lua_bin) results/dotfiles.zip results/binaries-linux-arm64.zip home/main.lua home/.args | results/bin
	$(call build_home,$@,linux-arm64)

results/bin/home-linux-x86_64: $(lua_bin) results/dotfiles.zip results/binaries-linux-x86_64.zip home/main.lua home/.args | results/bin
	$(call build_home,$@,linux-x86_64)

results/bin:
	mkdir -p $@

results:
	mkdir -p $@

home: results/bin/home-darwin-arm64 results/bin/home-linux-arm64 results/bin/home-linux-x86_64

.PHONY: build clean home
