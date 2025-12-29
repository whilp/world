include lib/spawn/cook.mk

version_file = lib/version.lua
home_exclude_pattern = ^(3p/|o/|results/|Makefile|lib/home/|\.git)
home_lua = LUA_PATH="$(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;$(CURDIR)/lib/home/?.lua;;" $(CURDIR)/$(lua_bin)
home_setup_dir = lib/home/setup
home_mac_dir = lib/home/mac
home_setup_sources = $(wildcard $(home_setup_dir)/*.lua)
home_mac_sources = $(wildcard $(home_mac_dir)/*.lua)

results/dotfiles.zip: private .UNVEIL = r:$(CURDIR) rx:$(cosmos_zip_bin) rwc:results rw:/dev/null
results/dotfiles.zip: private .PLEDGE = stdio rpath wpath cpath fattr exec proc
results/dotfiles.zip: $(cosmos_zip_bin) | results
	git ls-files -z | grep -zZvE '$(home_exclude_pattern)' | \
		xargs -0 $(cosmos_zip_bin) -q -r $@

define build_platform_asset
	@echo "Building platform asset $(1) for $(2)..."
	@rm -rf results/platform-$(2)
	@mkdir -p results/platform-$(2)/home/.local/share
	@echo "Extracting and organizing binaries..."
	@mkdir -p results/platform-$(2)/temp-binaries
	@unzip -q results/binaries-$(2).zip -d results/platform-$(2)/temp-binaries
	@cd results/platform-$(2)/temp-binaries && \
		for tool in $(TOOLS); do \
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
					mkdir -p "$$install_dir/bin"; \
					if [ -d "$$tool/$(2)/bin" ]; then \
						exe=$$(find "$$tool/$(2)/bin" -maxdepth 1 -type f -name "$$tool" 2>/dev/null | head -1); \
						if [ -n "$$exe" ]; then cp -p "$$exe" "$$install_dir/bin/$$tool"; fi; \
					else \
						exe=$$(find "$$tool/$(2)" -maxdepth 1 -type f -name "$$tool" 2>/dev/null | head -1); \
						if [ -n "$$exe" ]; then cp -p "$$exe" "$$install_dir/bin/$$tool"; fi; \
					fi; \
				fi; \
			fi; \
		done
	@rm -rf results/platform-$(2)/temp-binaries
	@echo "Generating manifest..."
	@$(home_lua) lib/home/gen-manifest.lua results/platform-$(2)/home $(HOME_VERSION) > results/platform-$(2)/manifest.lua
	@echo "Creating platform asset..."
	@cp $(lua_bin) $(1)
	@cd results/platform-$(2) && find . -type f -o -type l | $(cosmos_zip_bin) -q $(CURDIR)/$(1) -@
	@$(cosmos_zip_bin) -qj $(1) lib/home/main.lua
	@mkdir -p results/platform-$(2)/.lua && cp -r $(spawn_dir) results/platform-$(2)/.lua/ && cp $(version_file) results/platform-$(2)/.lua/
	@cd results/platform-$(2) && $(cosmos_zip_bin) -qr $(CURDIR)/$(1) .lua
	@echo -n '/zip/main.lua' > results/platform-$(2)/.args
	@$(cosmos_zip_bin) -qj $(1) results/platform-$(2)/.args
	@rm -rf results/platform-$(2)
endef

# generate platform asset rules
define platform_home_rule
results/bin/home-$(1): private .PLEDGE = stdio rpath wpath cpath fattr exec proc
results/bin/home-$(1): private .CPU = 120
results/bin/home-$(1): $$(lua_bin) results/binaries-$(1).zip lib/home/main.lua lib/home/gen-manifest.lua $$(spawn_sources) $$(version_file) | results/bin
	$$(call build_platform_asset,$$@,$(1))
endef

$(foreach p,$(PLATFORMS),$(eval $(call platform_home_rule,$(p))))

platform-assets: $(foreach p,$(PLATFORMS),results/bin/home-$(p)) ## Build platform-specific binaries

# universal home binary with dotfiles + platform metadata
HOME_VERSION ?= $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")
HOME_BASE_URL ?= https://github.com/whilp/dotfiles/releases/download/{tag}
HOME_TAG ?= home-$(shell date -u +%Y-%m-%d)-$(HOME_VERSION)

home_platform_deps := $(foreach p,$(PLATFORMS),results/bin/home-$(p))

results/bin/home: private .PLEDGE = stdio rpath wpath cpath fattr exec proc
results/bin/home: private .CPU = 180
results/bin/home: $(lua_bin) results/dotfiles.zip $(home_platform_deps) lib/home/main.lua lib/home/.args lib/home/gen-manifest.lua lib/home/gen-platforms.lua $(spawn_sources) $(version_file) $(home_setup_sources) $(home_mac_sources) | results/bin
	@echo "Building universal home binary..."
	@rm -rf results/home-universal
	@mkdir -p results/home-universal/home/.local/bin
	@echo "Extracting dotfiles..."
	@unzip -q results/dotfiles.zip -d results/home-universal/home
	@cp -p $(lua_bin) results/home-universal/home/.local/bin/lua
	@cp -p o/3p/cosmos/bin/unzip results/home-universal/home/.local/bin/unzip
	@echo "Generating manifest..."
	@$(home_lua) lib/home/gen-manifest.lua results/home-universal/home $(HOME_VERSION) > results/home-universal/manifest.lua
	@echo "Generating platforms metadata..."
	@$(home_lua) lib/home/gen-platforms.lua results/home-universal "$(HOME_BASE_URL)" "$(HOME_TAG)" $(home_platform_deps)
	@echo "Creating home binary..."
	@cp $(lua_bin) $@
	@cd results/home-universal && find . -type f -o -type l | $(cosmos_zip_bin) -q $(CURDIR)/$@ -@
	@cd lib/home && $(cosmos_zip_bin) -qr $(CURDIR)/$@ main.lua .args
	@mkdir -p results/home-universal/.lua && cp -r $(spawn_dir) $(home_setup_dir) $(home_mac_dir) lib/claude results/home-universal/.lua/ && cp $(version_file) results/home-universal/.lua/
	@cd results/home-universal && $(cosmos_zip_bin) -qr $(CURDIR)/$@ .lua
	@rm -rf results/home-universal

home: results/bin/home ## Build universal home binary

o/test/home.ok: private .UNVEIL = r:lib rx:$(lua_test) rwc:lib/home/o rw:/dev/null
o/test/home.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/test/home.ok: private .CPU = 60
o/test/home.ok: $(lua_test) lib/home/test_main.lua lib/home/main.lua
	@mkdir -p $(@D)
	$(lua_test) lib/home/test_main.lua
	@touch $@

.PHONY: home platform-assets
