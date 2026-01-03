modules += cosmic
cosmic_srcs := $(wildcard lib/cosmic/*.lua)
cosmic_tests := $(filter lib/cosmic/test_%.lua,$(cosmic_srcs))
cosmic_libs := $(addprefix $(o)/,$(filter-out $(cosmic_tests) lib/cosmic/lfs.lua,$(cosmic_srcs)))
cosmic_lfs := $(o)/lib/cosmic/lfs.lua
cosmic_bin := $(o)/bin/cosmic
cosmic_files := $(cosmic_bin) $(cosmic_libs) $(cosmic_lfs)
cosmic_deps := cosmos luaunit argparse

cosmic_built := $(o)/cosmic/.built

$(cosmic_bin): $(cosmic_libs) $(cosmic_lfs)
	@rm -rf $(cosmic_built)
	@mkdir -p $(cosmic_built)/.lua/cosmic $(@D)
	@$(cp) $(cosmic_libs) $(cosmic_built)/.lua/cosmic/
	@$(cp) $(cosmic_lfs) $(cosmic_built)/.lua/
	@$(cp) $(luaunit_dir)/*.lua $(cosmic_built)/.lua/
	@$(cp) $(argparse_dir)/*.lua $(cosmic_built)/.lua/
	@$(cp) $(cosmos_lua) $@
	@chmod +x $@
	@cd $(cosmic_built) && $(CURDIR)/$(cosmos_zip) -qr $(CURDIR)/$@ .lua
