modules += box
box_srcs := $(wildcard lib/box/*.tl)
box_tl_files := $(box_srcs)
box_bin := $(o)/bin/box
box_files := $(box_bin)
box_tests := $(wildcard lib/box/test_*.tl)
box_deps := cosmos cosmic

# Build staging directory
box_built := $(o)/box/.built

# Compiled .lua from box_tl_files
box_tl_lua := $(patsubst lib/%.tl,$(o)/lib/%.lua,$(box_tl_files))

# Box binary: self-bootstrapping remote environment manager
# Bundles: box lua modules, cosmic/spawn, zip/unzip tools, claude version
$(box_bin): $(box_tl_lua) $$(cosmos_staged) $$(cosmic_bin) $$(cosmic_tl_libs) lib/claude/version.lua
	@rm -rf $(box_built)
	@mkdir -p $(box_built)/.lua/box $(box_built)/.lua/cosmic $(box_built)/.lua/claude $(@D)
	@$(cp) $(o)/lib/box/*.lua $(box_built)/.lua/box/
	@$(cp) $(cosmic_tl_libs) $(box_built)/.lua/cosmic/
	@$(cp) lib/claude/version.lua $(box_built)/.lua/claude/
	@$(cp) $(cosmos_dir)/zip $(box_built)/zip
	@$(cp) $(cosmos_dir)/unzip $(box_built)/unzip
	@$(cp) $(cosmos_lua) $@
	@chmod +x $@
	@cd $(box_built) && $(CURDIR)/$(cosmos_zip) -qr $(CURDIR)/$@ .lua zip unzip
	@$(cosmos_zip) -qj $@ $(o)/lib/box/init.lua
	@echo '/zip/init.lua' > $(box_built)/.args
	@cd $(box_built) && $(CURDIR)/$(cosmos_zip) -qj $(CURDIR)/$@ .args
	@rm -rf $(box_built)

box: $(box_bin)

.PHONY: box

# backend test depends on example-backend
$(o)/lib/box/test_backend.tl.test.ok: lib/box/example-backend
