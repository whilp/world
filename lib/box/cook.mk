modules += box
box_srcs := $(wildcard lib/box/*.tl)
box_tl_files := $(box_srcs)
box_bin := $(o)/bin/box
box_sprite := $(o)/bin/box-sprite
box_files := $(box_bin) $(box_sprite)
box_tests := $(wildcard lib/box/test_*.tl)
box_deps := cosmos cosmic

# Build staging directory
box_built := $(o)/box/.built

# Compiled .lua from box_tl_files
box_tl_lua := $(patsubst lib/%.tl,$(o)/lib/%.lua,$(box_tl_files))

# Box binary: uses prebuilt cosmic which has cosmic modules bundled
# Bundles: box lua modules, zip/unzip tools, claude version
# TODO: use cosmic --embed when available
$(box_bin): $(box_tl_lua) $$(cosmos_staged) $$(cosmic_staged) lib/claude/version.lua
	@rm -rf $(box_built)
	@mkdir -p $(box_built)/.lua/box $(box_built)/.lua/claude $(@D)
	@$(cp) $(o)/lib/box/*.lua $(box_built)/.lua/box/
	@$(cp) lib/claude/version.lua $(box_built)/.lua/claude/
	@$(cp) $(cosmos_dir)/zip $(box_built)/zip
	@$(cp) $(cosmos_dir)/unzip $(box_built)/unzip
	@$(cp) $(cosmic_dir)/bin/cosmic $@
	@chmod +x $@
	@cd $(box_built) && $(CURDIR)/$(cosmos_zip) -qr $(CURDIR)/$@ .lua zip unzip
	@$(cosmos_zip) -qj $@ $(o)/lib/box/init.lua
	@echo '/zip/init.lua' > $(box_built)/.args
	@cd $(box_built) && $(CURDIR)/$(cosmos_zip) -qj $(CURDIR)/$@ .args
	@rm -rf $(box_built)

box: $(box_bin)

.PHONY: box

# box-sprite backend executable
$(box_sprite): lib/box/box-sprite
	@mkdir -p $(@D)
	@$(cp) $< $@
	@chmod +x $@
