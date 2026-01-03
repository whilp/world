modules += cosmic
cosmic_bin := $(o)/bin/cosmic
cosmic_files := $(addprefix $(o)/lib/cosmic/,init.lua spawn.lua walk.lua help.lua)
cosmic_tests := $(wildcard lib/cosmic/test_*.lua)

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
