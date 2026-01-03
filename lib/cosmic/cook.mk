modules += cosmic
cosmic_srcs := $(wildcard lib/cosmic/*.lua)
cosmic_tests := $(filter lib/cosmic/test_%.lua,$(cosmic_srcs))
cosmic_libs := $(addprefix $(o)/,$(filter-out $(cosmic_tests),$(cosmic_srcs)))
cosmic_bin := $(o)/bin/cosmic
cosmic_real := $(o)/bin/cosmic-real
cosmic_files := $(cosmic_bin) $(cosmic_real) $(cosmic_libs)
cosmic_deps := cosmos luaunit argparse

cosmic_built := $(o)/cosmic/.built

# TODO: build cosmic properly from cosmic_libs
$(cosmic_bin): $(bootstrap_cosmic)
	@mkdir -p $(@D)
	@$(cp) $< $@

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
