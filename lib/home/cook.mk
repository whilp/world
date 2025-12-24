home_exclude_pattern = ^(3p/|o/|results/|Makefile|lib/home/|\.git)

results/dotfiles.zip: private .UNVEIL = \
	r:$(CURDIR) \
	rx:$(cosmos_zip_bin) \
	rwc:results \
	rw:/dev/null
results/dotfiles.zip: $(cosmos_zip_bin) | results
	git ls-files -z | grep -zZvE '$(home_exclude_pattern)' | \
		xargs -0 $(cosmos_zip_bin) -q -r $@

# Platform-specific binary assets (home-darwin-arm64, home-linux-arm64, home-linux-x86_64)
define build_platform_asset
	@echo "Building platform asset $(1) for $(2)..."
	@rm -rf results/platform-$(2)
	@mkdir -p results/platform-$(2)/home/.local/share
	@echo "Extracting and organizing binaries..."
	@mkdir -p results/platform-$(2)/temp-binaries
	@unzip -q results/binaries-$(2).zip -d results/platform-$(2)/temp-binaries
	@cd results/platform-$(2)/temp-binaries && \
		for tool in nvim gh delta rg duckdb tree-sitter ast-grep biome comrak marksman ruff shfmt sqruff stylua superhtml uv; do \
			if [ -d "$$tool/$(2)" ]; then \
				version=$$(cat "$$tool/$(2)/VERSION" 2>/dev/null || echo "0.0.0"); \
				version=$${version:-0.0.0}; \
				sha=$$(cat "$$tool/$(2)/SHA" 2>/dev/null || echo ""); \
				sha=$$(echo "$$sha" | head -c 8); \
				sha=$${sha:-00000000}; \
				install_dir="$(CURDIR)/results/platform-$(2)/home/.local/share/$$tool/$${version}-$${sha}"; \
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
	@rm -rf results/platform-$(2)/temp-binaries
	@echo "Generating manifest..."
	@LUA_PATH="lib/home/?.lua;;" $(lua_bin) lib/home/gen-manifest.lua results/platform-$(2)/home $(HOME_VERSION) > results/platform-$(2)/manifest.lua
	@echo "Creating platform asset..."
	@cp $(lua_bin) $(1)
	@cd results/platform-$(2) && find . -type f -o -type l | $(cosmos_zip_bin) -q $(CURDIR)/$(1) -@
	@$(cosmos_zip_bin) -qj $(1) lib/home/main.lua
	@echo -n '/zip/main.lua' > results/platform-$(2)/.args
	@$(cosmos_zip_bin) -qj $(1) results/platform-$(2)/.args
	@rm -rf results/platform-$(2)
endef

results/bin/home-darwin-arm64: $(lua_bin) results/binaries-darwin-arm64.zip lib/home/main.lua lib/home/gen-manifest.lua | results/bin
	$(call build_platform_asset,$@,darwin-arm64)

results/bin/home-linux-arm64: $(lua_bin) results/binaries-linux-arm64.zip lib/home/main.lua lib/home/gen-manifest.lua | results/bin
	$(call build_platform_asset,$@,linux-arm64)

results/bin/home-linux-x86_64: $(lua_bin) results/binaries-linux-x86_64.zip lib/home/main.lua lib/home/gen-manifest.lua | results/bin
	$(call build_platform_asset,$@,linux-x86_64)

platform-assets: results/bin/home-darwin-arm64 results/bin/home-linux-arm64 results/bin/home-linux-x86_64

# Universal home binary with dotfiles + platform metadata
HOME_VERSION ?= $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")
HOME_BASE_URL ?= https://github.com/whilp/dotfiles/releases/download/$${tag}
HOME_TAG ?= home-$(shell date -u +%Y-%m-%d)-$(HOME_VERSION)

results/bin/home: $(lua_bin) results/dotfiles.zip results/bin/home-darwin-arm64 results/bin/home-linux-arm64 results/bin/home-linux-x86_64 lib/home/main.lua lib/home/.args lib/home/gen-manifest.lua lib/home/gen-platforms.lua | results/bin
	@echo "Building universal home binary..."
	@rm -rf results/home-universal
	@mkdir -p results/home-universal/home/.local/bin
	@echo "Extracting dotfiles..."
	@unzip -q results/dotfiles.zip -d results/home-universal/home
	@cp -p $(lua_bin) results/home-universal/home/.local/bin/lua
	@cp -p o/3p/cosmos/bin/unzip results/home-universal/home/.local/bin/unzip
	@echo "Generating manifest..."
	@LUA_PATH="lib/home/?.lua;;" $(lua_bin) lib/home/gen-manifest.lua results/home-universal/home $(HOME_VERSION) > results/home-universal/manifest.lua
	@echo "Generating platforms metadata..."
	# Single quotes preserve ${tag} literally; double quotes would expand it as empty shell variable
	@LUA_PATH="lib/home/?.lua;;" $(lua_bin) lib/home/gen-platforms.lua results/home-universal '$(HOME_BASE_URL)' "$(HOME_TAG)" \
		results/bin/home-darwin-arm64 \
		results/bin/home-linux-arm64 \
		results/bin/home-linux-x86_64
	@echo "Creating home binary..."
	@cp $(lua_bin) $@
	@cd results/home-universal && find . -type f -o -type l | $(cosmos_zip_bin) -q $(CURDIR)/$@ -@
	@cd lib/home && $(cosmos_zip_bin) -qr $(CURDIR)/$@ main.lua .args
	@rm -rf results/home-universal

home: results/bin/home

.PHONY: home platform-assets
