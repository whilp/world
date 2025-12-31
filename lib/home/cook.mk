include lib/spawn/cook.mk

version_file = lib/version.lua
home_exclude_pattern = ^(3p/|o/|Makefile|lib/home/|\.git)
home_setup_dir = lib/home/setup
home_mac_dir = lib/home/mac
home_setup_sources = $(wildcard $(home_setup_dir)/*.lua)
home_mac_sources = $(wildcard $(home_mac_dir)/*.lua)

$(o)/dotfiles.zip: private .UNVEIL = r:$(CURDIR) rx:$(cosmos_zip_bin) rwc:$(o) rw:/dev/null
$(o)/dotfiles.zip: private .PLEDGE = stdio rpath wpath cpath fattr exec proc
$(o)/dotfiles.zip: $(cosmos_zip_bin)
	git ls-files -z | grep -zZvE '$(home_exclude_pattern)' | \
		xargs -0 $(cosmos_zip_bin) -q -r $@

install_tools := lib/build/install-tools.lua

define build_platform_asset
	@echo "Building platform asset $(1) for $(2)..."
	@rm -rf $(o)/platform-$(2)
	@mkdir -p $(o)/platform-$(2)/home/.local/share
	@echo "Installing tools..."
	@$(lua_bin) $(install_tools) $(o)/binaries-$(2).zip $(2) $(o)/platform-$(2)
	@echo "Generating manifest..."
	@$(lua_bin) lib/home/gen-manifest.lua $(o)/platform-$(2)/home $(HOME_VERSION) > $(o)/platform-$(2)/manifest.lua
	@echo "Creating platform asset..."
	@cp $(lua_bin) $(1)
	@cd $(o)/platform-$(2) && find . -type f -o -type l | $(cosmos_zip_bin) -q $(1) -@
	@$(cosmos_zip_bin) -qj $(1) lib/home/main.lua
	@mkdir -p $(o)/platform-$(2)/.lua && cp -r $(spawn_dir) $(o)/platform-$(2)/.lua/ && cp $(version_file) $(o)/platform-$(2)/.lua/
	@cd $(o)/platform-$(2) && $(cosmos_zip_bin) -qr $(1) .lua
	@echo -n '/zip/main.lua' > $(o)/platform-$(2)/.args
	@$(cosmos_zip_bin) -qj $(1) $(o)/platform-$(2)/.args
	@rm -rf $(o)/platform-$(2)
endef

# generate platform asset rules
define platform_home_rule
$(o)/$(1)/bin/home: private .PLEDGE = stdio rpath wpath cpath fattr exec proc
$(o)/$(1)/bin/home: private .CPU = 120
$(o)/$(1)/bin/home: $$(lua_bin) $(o)/binaries-$(1).zip lib/home/main.lua lib/home/gen-manifest.lua $$(spawn_sources) $$(version_file) | $(o)/$(1)/bin
	$$(call build_platform_asset,$$@,$(1))

$(o)/$(1)/bin:
	@mkdir -p $$@
endef

$(foreach p,$(PLATFORMS),$(eval $(call platform_home_rule,$(p))))

platform-assets: $(foreach p,$(PLATFORMS),$(o)/$(p)/bin/home) ## Build platform-specific binaries

# universal home binary with dotfiles + platform metadata
HOME_VERSION ?= $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")
HOME_BASE_URL ?= https://github.com/whilp/dotfiles/releases/download/{tag}
HOME_TAG ?= home-$(shell date -u +%Y-%m-%d)-$(HOME_VERSION)

home_platform_deps := $(foreach p,$(PLATFORMS),$(o)/$(p)/bin/home)

$(o)/any/bin/home: private .PLEDGE = stdio rpath wpath cpath fattr exec proc
$(o)/any/bin/home: private .CPU = 180
$(o)/any/bin/home: $(lua_bin) $(cosmos_unzip_bin) $(o)/dotfiles.zip $(home_platform_deps) lib/home/main.lua lib/home/.args lib/home/gen-manifest.lua lib/home/gen-platforms.lua $(spawn_sources) $(version_file) $(home_setup_sources) $(home_mac_sources) | o/any/bin
	@echo "Building universal home binary..."
	@rm -rf $(o)/home-universal
	@mkdir -p $(o)/home-universal/home/.local/bin
	@echo "Extracting dotfiles..."
	@unzip -q $(o)/dotfiles.zip -d $(o)/home-universal/home
	@cp -p $(lua_bin) $(o)/home-universal/home/.local/bin/lua
	@cp -p $(cosmos_unzip_bin) $(o)/home-universal/home/.local/bin/unzip
	@echo "Generating manifest..."
	@$(lua_bin) lib/home/gen-manifest.lua $(o)/home-universal/home $(HOME_VERSION) > $(o)/home-universal/manifest.lua
	@echo "Generating platforms metadata..."
	@$(lua_bin) lib/home/gen-platforms.lua $(o)/home-universal "$(HOME_BASE_URL)" "$(HOME_TAG)" $(home_platform_deps)
	@echo "Creating home binary..."
	@cp $(lua_bin) $@
	@cd $(o)/home-universal && find . -type f -o -type l | $(cosmos_zip_bin) -q $@ -@
	@cd lib/home && $(cosmos_zip_bin) -qr $@ main.lua .args
	@mkdir -p $(o)/home-universal/.lua && cp -r $(spawn_dir) $(home_setup_dir) $(home_mac_dir) lib/claude $(o)/home-universal/.lua/ && cp $(version_file) $(o)/home-universal/.lua/
	@cd $(o)/home-universal && $(cosmos_zip_bin) -qr $@ .lua
	@rm -rf $(o)/home-universal

home: $(o)/any/bin/home ## Build universal home binary

o/lib/home/test_main.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rw:/dev/null
o/lib/home/test_main.lua.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/lib/home/test_main.lua.ok: private .CPU = 60
o/lib/home/test_main.lua.ok: $(lua_test) lib/home/test_main.lua lib/home/main.lua
	@mkdir -p $(@D)
	$(lua_test) lib/home/test_main.lua
	@touch $@

.PHONY: home platform-assets
