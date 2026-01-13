modules += clasp
clasp_version := 3p/clasp/version.lua
clasp_bin := $(o)/bin/clasp
clasp_files := $(clasp_bin)
clasp_tests := 3p/clasp/test_clasp.tl
clasp_deps := bun

$(clasp_bin): $$(clasp_staged) $$(bun_staged)
	@mkdir -p $(@D)
	@cd $(clasp_dir) && $(CURDIR)/$(bun_dir)/bin/bun install
	@cd $(clasp_dir) && $(CURDIR)/$(bun_dir)/bin/bun build --compile src/index.ts --outfile $(CURDIR)/$@
