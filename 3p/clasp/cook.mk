modules += clasp
clasp_version := 3p/clasp/version.lua
clasp_bin := $(o)/bin/clasp
clasp_files := $(clasp_bin)
clasp_tests := 3p/clasp/test_clasp.tl

$(clasp_bin): $(clasp_staged) | $(o)/bin
	@ln -sf $(shell realpath $(clasp_dir)/clasp) $@
