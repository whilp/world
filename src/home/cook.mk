home_exclude_pattern = ^(3p/|o/|results/|Makefile|src/home/|\.git)

results/dotfiles.zip: private .UNVEIL = \
	r:$(CURDIR) \
	rx:$(cosmos_zip_bin) \
	rwc:results \
	rw:/dev/null
results/dotfiles.zip: $(cosmos_zip_bin) | results
	git ls-files -z | grep -zZvE '$(home_exclude_pattern)' | \
		xargs -0 $(cosmos_zip_bin) -q -r $@

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
				version=$$(cat "$$tool/$(2)/VERSION" 2>/dev/null || echo "0.0.0"); \
				sha=$$(cat "$$tool/$(2)/SHA" 2>/dev/null | head -c 8 || echo "00000000"); \
				install_dir="$(CURDIR)/results/home-$(2)/home/.local/share/$$tool/$${version}-$${sha}"; \
				echo "  Installing $$tool $${version}-$${sha}..."; \
				mkdir -p "$$install_dir"; \
				if [ "$$tool" = "nvim" ] || [ "$$tool" = "gh" ]; then \
					cp -r $$tool/$(2)/bin $$tool/$(2)/lib $$tool/$(2)/share "$$install_dir/" 2>/dev/null || true; \
					cp -r $$tool/$(2)/libexec "$$install_dir/" 2>/dev/null || true; \
				else \
					if [ -d "$$tool/$(2)/bin" ]; then \
						exe=$$(find "$$tool/$(2)/bin" -maxdepth 1 -type f -name "$$tool" 2>/dev/null | head -1); \
						if [ -n "$$exe" ]; then cp -p "$$exe" "$$install_dir/$$tool"; fi; \
					else \
						exe=$$(find "$$tool/$(2)" -maxdepth 1 -type f -name "$$tool" 2>/dev/null | head -1); \
						if [ -n "$$exe" ]; then cp -p "$$exe" "$$install_dir/$$tool"; fi; \
					fi; \
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
	@cd src/home && $(cosmos_zip_bin) -qr $(CURDIR)/$(1) main.lua .args
	@rm -rf results/home-$(2)
endef

results/bin/home-darwin-arm64: $(lua_bin) results/dotfiles.zip results/binaries-darwin-arm64.zip src/home/main.lua src/home/.args | results/bin
	$(call build_home,$@,darwin-arm64)

results/bin/home-linux-arm64: $(lua_bin) results/dotfiles.zip results/binaries-linux-arm64.zip src/home/main.lua src/home/.args | results/bin
	$(call build_home,$@,linux-arm64)

results/bin/home-linux-x86_64: $(lua_bin) results/dotfiles.zip results/binaries-linux-x86_64.zip src/home/main.lua src/home/.args | results/bin
	$(call build_home,$@,linux-x86_64)

home: results/bin/home-darwin-arm64 results/bin/home-linux-arm64 results/bin/home-linux-x86_64

.PHONY: home
