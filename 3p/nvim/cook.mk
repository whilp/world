modules += nvim
nvim_version := 3p/nvim/version.lua
nvim_deps := nvim-conform nvim-mini nvim-lspconfig nvim-treesitter nvim-parsers

# Basic test only needs raw binary (nvim_dir defaults to nvim_staged)
nvim_tests := 3p/nvim/test_nvim.tl

# Bundle tests run during release (need full bundle)
nvim_release_tests := $(filter-out $(nvim_tests),$(wildcard 3p/nvim/test_*.tl))
nvim_release_tested := $(patsubst %,$(o)/%.release.ok,$(nvim_release_tests))

# Bundle output - used by home binary and release tests
nvim_bundle := $(o)/nvim/.bundle
nvim_bundle_out := $(o)/bundled/nvim
nvim_pack_dir := $(nvim_bundle_out)/share/nvim/site/pack/core/opt

# nvim-bundle: build the bundle (used by release/build targets)
.PHONY: nvim-bundle
nvim-bundle: $(nvim_bundle)

$(nvim_bundle): $$(nvim_staged) $$(nvim-conform_staged) $$(nvim-mini_staged) $$(nvim-lspconfig_staged) $$(nvim-treesitter_staged) $$(nvim-parsers_parsers) $$(cosmic_bin)
	@rm -rf $(nvim_bundle_out) $@
	@mkdir -p $(dir $(nvim_bundle_out))
	@cp -rL $(nvim_staged) $(nvim_bundle_out)
	@mkdir -p $(nvim_pack_dir)
	@cp -rL $(nvim-conform_staged) $(nvim_pack_dir)/conform.nvim
	@cp -rL $(nvim-mini_staged) $(nvim_pack_dir)/mini.nvim
	@cp -rL $(nvim-lspconfig_staged) $(nvim_pack_dir)/nvim-lspconfig
	@cp -rL $(nvim-treesitter_staged) $(nvim_pack_dir)/nvim-treesitter
	@cp -r $(o)/nvim-parsers/parser $(nvim_bundle_out)/share/nvim/site/
	@unzip -p $(cosmic_bin) .lua/tl.lua > $(nvim_bundle_out)/share/nvim/runtime/lua/tl.lua
	@echo "generating helptags"
	@$(nvim_bundle_out)/bin/nvim --headless "+helptags ALL" "+qa" 2>/dev/null || echo "  skipped"
	@touch $@

# nvim-release-tests: run bundle tests (used by test-release target)
.PHONY: nvim-release-tests
nvim-release-tests: $(nvim_release_tested)

# Release tests: depend on compiled .lua (Make handles compilation)
$(o)/3p/nvim/%.tl.release.ok: $(o)/3p/nvim/%.lua $(nvim_bundle) | $(bootstrap_files)
	@mkdir -p $(@D)
	@[ -x $< ] || chmod a+x $<
	@TEST_DIR=$(nvim_bundle_out) $(test_runner) $< > $@
