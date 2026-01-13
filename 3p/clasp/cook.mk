modules += clasp
clasp_version := 3p/clasp/version.lua
clasp_deps := bun
clasp_bin := $(o)/bin/clasp
clasp_files := $(clasp_bin)
clasp_tests := 3p/clasp/test_clasp.tl

$(clasp_bin): $(clasp_staged) $(bun_staged) 3p/clasp/clasp.sh | $(o)/bin
	@cp 3p/clasp/clasp.sh $@
	@sed -i 's|__BUN__|$(shell realpath $(bun_dir)/bun)|g' $@
	@sed -i 's|__CLASP_DIR__|$(shell realpath $(clasp_dir))|g' $@
	@chmod +x $@
