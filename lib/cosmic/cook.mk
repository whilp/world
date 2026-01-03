modules += cosmic
cosmic_bin := $(o)/bin/cosmic
cosmic_real := $(o)/bin/cosmic-real
cosmic_libs := $(addprefix $(o)/lib/cosmic/,init.lua spawn.lua walk.lua help.lua)
cosmic_files := $(cosmic_bin) $(cosmic_real) $(cosmic_libs)
cosmic_tests := $(wildcard lib/cosmic/test_*.lua)
cosmic_deps := cosmos luaunit argparse

# TODO: build cosmic properly
$(cosmic_bin): $(bootstrap_cosmic)
	@mkdir -p $(@D)
	@$(cp) $< $@

cosmic_staging := $(o)/cosmic-staging

$(cosmic_real): $(cosmic_libs)
	@rm -rf $(cosmic_staging)
	@mkdir -p $(cosmic_staging)/.lua/cosmic $(@D)
	@cp $(o)/lib/cosmic/*.lua $(cosmic_staging)/.lua/cosmic/
	@cp $(luaunit_dir)/luaunit.lua $(cosmic_staging)/.lua/
	@cp $(argparse_dir)/src/argparse.lua $(cosmic_staging)/.lua/
	@cp 3p/lua/lfs_stub.lua $(cosmic_staging)/.lua/lfs.lua
	@cp $(cosmos_dir)/lua $@
	@chmod +x $@
	@cd $(cosmic_staging) && $(CURDIR)/$(cosmos_dir)/zip -qr $(CURDIR)/$@ .lua

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
