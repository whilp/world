modules += cosmic
cosmic_bin := $(o)/bin/cosmic
cosmic_real := $(o)/bin/cosmic-real
cosmic_srcs := $(wildcard lib/cosmic/*.lua)
cosmic_tests := $(filter lib/cosmic/test_%.lua,$(cosmic_srcs))
cosmic_libs := $(addprefix $(o)/,$(filter-out $(cosmic_tests),$(cosmic_srcs)))
cosmic_files := $(cosmic_bin) $(cosmic_real) $(cosmic_libs)
cosmic_deps := cosmos luaunit argparse

# TODO: build cosmic properly
$(cosmic_bin): $(bootstrap_cosmic)
	@mkdir -p $(@D)
	@$(cp) $< $@

cosmic_built := $(o)/cosmic/.built

$(cosmic_real): $(cosmic_libs)
	@rm -rf $(cosmic_built)
	@mkdir -p $(cosmic_built)/.lua/cosmic $(@D)
	@$(cp) $(cosmic_libs) $(cosmic_built)/.lua/cosmic/
	@$(cp) $(luaunit_dir)/*.lua $(cosmic_built)/.lua/
	@$(cp) $(argparse_dir)/*.lua $(cosmic_built)/.lua/
	@$(cp) 3p/lua/lfs_stub.lua $(cosmic_built)/.lua/lfs.lua
	@$(cp) $(cosmos_dir)/lua $@
	@chmod +x $@
	@cd $(cosmic_built) && $(CURDIR)/$(cosmos_dir)/zip -qr $(CURDIR)/$@ .lua

# cosmic binary

#$(cosmic_bin): $(o)/$(platform)/cosmos/bin/lua $(o)/$(platform)/cosmos/bin/zip $(cosmic_files) $(luaunit_staged) $(o)/$(platform)/argparse/lib/argparse.lua $(o)/$(platform)/lfs/lib/lfs.lua
#	@rm -rf $(o)/any/cosmic/staging
#	@mkdir -p $(o)/any/cosmic/staging/.lua $(@D)
#	@cp -r $(o)/lib/cosmic/* $(o)/any/cosmic/staging/.lua/
#	@cp -r $(luaunit_staged)/lib/* $(o)/any/cosmic/staging/.lua/
#	@cp -r $(o)/$(platform)/argparse/lib/* $(o)/any/cosmic/staging/.lua/
#	@cp -r $(o)/$(platform)/lfs/lib/* $(o)/any/cosmic/staging/.lua/
#	@cp $(o)/$(platform)/cosmos/bin/lua $@
#	@chmod +x $@
#	@cd $(o)/any/cosmic/staging && $(CURDIR)/$(o)/$(platform)/cosmos/bin/zip -qr $(CURDIR)/$@ .lua
#
#.PHONY: cosmic
#cosmic: $(cosmic_bin) ## Build cosmic binary
