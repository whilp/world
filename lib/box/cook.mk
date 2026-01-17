modules += box
box_srcs := $(wildcard lib/box/*.lua) $(wildcard lib/box/*.tl)
box_tl_files := lib/box/backend.tl lib/box/init.tl lib/box/run.tl lib/box/sprite.tl lib/box/mac.tl
box_bin := $(o)/bin/box
box_files := $(box_bin)
box_tests := $(wildcard lib/box/test_*.tl)
box_deps := cosmos cosmic home

# Build staging directory
box_built := $(o)/box/.built

# Compiled .lua from box_tl_files
box_tl_lua := $(patsubst %.tl,$(o)/%.lua,$(box_tl_files))

# Box binary: self-bootstrapping remote environment manager
# Bundles: box lua modules, home/bootstrap.lua for remote run, cosmic/spawn, zip/unzip tools
$(box_bin): $(box_tl_lua) $$(cosmos_staged) $$(cosmic_bin) $$(cosmic_tl_libs)
	@rm -rf $(box_built)
	@mkdir -p $(box_built)/.lua/box $(box_built)/.lua/cosmic $(box_built)/.lua/home $(@D)
	@$(cp) $(o)/lib/box/*.lua $(box_built)/.lua/box/
	@$(cp) $(cosmic_tl_libs) $(box_built)/.lua/cosmic/
	@$(cp) $(o)/lib/home/bootstrap.lua $(box_built)/.lua/home/
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
