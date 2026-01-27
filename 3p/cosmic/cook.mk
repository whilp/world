# TODO: use cosmic --embed when available
modules += cosmic
cosmic_version := 3p/cosmic/version.lua
cosmic_bin := $(o)/bin/cosmic
cosmic_files := $(cosmic_bin)
cosmic_deps := cosmos skill
cosmic_tests := $(wildcard 3p/cosmic/test_*.tl)

cosmic_built := $(o)/cosmic/.built

# Build cosmic by appending world's skill modules to prebuilt cosmic-lua
# TODO: use cosmic --embed when available
$(cosmic_bin): $(skill_libs) $$(cosmic_staged) $$(cosmos_staged)
	@rm -rf $(cosmic_built)
	@mkdir -p $(cosmic_built)/.lua/skill $(@D)
	@$(cp) $(skill_libs) $(cosmic_built)/.lua/skill/
	@$(cp) $(cosmic_dir)/bin/cosmic $@
	@chmod +x $@
	@cd $(cosmic_built) && $(CURDIR)/$(cosmos_zip) -qr $(CURDIR)/$@ .lua

cosmic: $(cosmic_bin)

.PHONY: cosmic
