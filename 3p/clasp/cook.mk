modules += clasp
clasp_srcs := 3p/clasp/clasp.ts
clasp_bin := $(o)/bin/clasp
clasp_files := $(clasp_bin)
clasp_tests := 3p/clasp/test_clasp.tl
clasp_deps := bun

$(clasp_bin): $(clasp_srcs) $$(bun_staged)
	@mkdir -p $(@D)
	@$(bun_dir)/bin/bun build --compile $(clasp_srcs) --outfile $@
