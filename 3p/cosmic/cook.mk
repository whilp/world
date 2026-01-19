modules += cosmic-bin
cosmic-bin_version := 3p/cosmic/version.lua

modules += cosmic
cosmic_bin := $(o)/bin/cosmic
cosmic_files := $(cosmic_bin)
cosmic_deps := cosmic-bin skill

cosmic_built := $(o)/cosmic/.built

# Build cosmic by appending world's skill modules to prebuilt cosmic-lua
$(cosmic_bin): $(skill_libs) $$(cosmic-bin_staged)
	@rm -rf $(cosmic_built)
	@mkdir -p $(cosmic_built)/.lua/skill $(@D)
	@$(cp) $(skill_libs) $(cosmic_built)/.lua/skill/
	@$(cp) $(cosmic-bin_dir)/bin/cosmic-bin $@
	@chmod +x $@
	@cd $(cosmic_built) && $(CURDIR)/$(cosmos_zip) -qr $(CURDIR)/$@ .lua

cosmic: $(cosmic_bin)

.PHONY: cosmic
