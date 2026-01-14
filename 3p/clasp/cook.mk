modules += clasp
clasp_version := 3p/clasp/version.lua
clasp_bin := $(o)/bin/clasp
clasp_files := $(clasp_bin)
clasp_tests := 3p/clasp/test_clasp.tl
clasp_deps := bun

clasp_lock := 3p/clasp/bun.lock

$(clasp_bin): $$(clasp_staged) $$(bun_staged)
	@mkdir -p $(@D)
	@rm -f $(clasp_dir)/package-lock.json
	@if [ -f $(clasp_lock) ]; then \
		cp $(clasp_lock) $(clasp_dir)/bun.lock; \
		cd $(clasp_dir) && $(CURDIR)/$(bun_dir)/bin/bun install --ignore-scripts --frozen-lockfile; \
	else \
		cd $(clasp_dir) && $(CURDIR)/$(bun_dir)/bin/bun install --ignore-scripts --save-text-lockfile; \
		cp $(clasp_dir)/bun.lock $(CURDIR)/$(clasp_lock); \
	fi
	@cd $(clasp_dir) && $(CURDIR)/$(bun_dir)/bin/bun build --compile src/index.ts --outfile $(CURDIR)/$@
