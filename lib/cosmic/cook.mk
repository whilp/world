modules += cosmic
cosmic_lua_srcs := $(wildcard lib/cosmic/*.lua)
cosmic_tl_srcs := $(wildcard lib/cosmic/*.tl)
cosmic_srcs := $(cosmic_lua_srcs) $(cosmic_tl_srcs)
cosmic_tests := $(filter lib/cosmic/test_%.tl,$(cosmic_tl_srcs))
cosmic_main := $(o)/lib/cosmic/main.lua
cosmic_args := lib/cosmic/.args
cosmic_tl_gen := lib/cosmic/tl-gen.lua
# lua sources: filter out tests, main, and tl-gen, then add o/ prefix
cosmic_lua_libs := $(addprefix $(o)/,$(filter-out $(cosmic_tests) $(cosmic_main) $(cosmic_tl_gen),$(cosmic_lua_srcs)))
# tl sources: compile to .lua in o/ (excluding main which is bundled separately)
cosmic_tl_libs := $(filter-out $(cosmic_main),$(patsubst %.tl,$(o)/%.lua,$(cosmic_tl_srcs)))
cosmic_libs := $(cosmic_lua_libs) $(cosmic_tl_libs)
cosmic_bin := $(o)/bin/cosmic
cosmic_files := $(cosmic_bin) $(cosmic_libs)
cosmic_deps := cosmos skill tl teal-types

cosmic_built := $(o)/cosmic/.built

$(cosmic_bin): $(cosmic_libs) $(skill_libs) $(cosmic_main) $(cosmic_args) $(cosmic_tl_gen) $$(tl_staged) $$(teal-types_staged)
	@rm -rf $(cosmic_built)
	@mkdir -p $(cosmic_built)/.lua/cosmic $(cosmic_built)/.lua/skill $(@D)
	@$(cp) $(cosmic_libs) $(cosmic_built)/.lua/cosmic/
	@$(cp) $(skill_libs) $(cosmic_built)/.lua/skill/
	@$(cp) $(tl_dir)/tl.lua $(cosmic_built)/.lua/
	@cp -r $(teal-types_dir)/types $(cosmic_built)/.lua/teal-types
	@$(cp) $(cosmos_lua) $@
	@chmod +x $@
	@cd $(cosmic_built) && $(CURDIR)/$(cosmos_zip) -qr $(CURDIR)/$@ .lua
	@$(cosmos_zip) -qj $@ $(cosmic_main) $(cosmic_args) $(cosmic_tl_gen)

cosmic: $(cosmic_bin)

.PHONY: cosmic
