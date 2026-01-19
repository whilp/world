modules += clasp
clasp_version := 3p/clasp/version.lua
clasp_bin := $(o)/bin/clasp
clasp_files := $(clasp_bin)
clasp_tests := 3p/clasp/test_clasp.tl
clasp_deps := bun

clasp_lock := 3p/clasp/bun.lock

.PHONY: clasp-lock
## Generate clasp bun.lock for reproducible builds
clasp-lock: $$(clasp_staged) $$(bun_staged)
	@rm -f $(clasp_dir)/package-lock.json $(clasp_dir)/bun.lock
	@cd $(clasp_dir) && $(CURDIR)/$(bun_dir)/bin/bun install --ignore-scripts --save-text-lockfile
	@cp $(clasp_dir)/bun.lock $(clasp_lock)
	@echo "Generated $(clasp_lock)"

$(clasp_bin): $$(clasp_staged) $$(bun_staged) $(clasp_lock)
	@mkdir -p $(@D)
	@rm -f $(clasp_dir)/package-lock.json
	@cp $(clasp_lock) $(clasp_dir)/bun.lock
	@cd $(clasp_dir) && $(CURDIR)/$(bun_dir)/bin/bun install --ignore-scripts --frozen-lockfile >/dev/null
	@perl -i -pe 's/var msgId = messageDescriptor\.id(?! \|\|)/var msgId = messageDescriptor.id || "auto"/' $(clasp_dir)/node_modules/@formatjs/intl/src/message.js
	@perl -i -pe 's/var msgId = messageDescriptor\.id(?! \|\|)/var msgId = messageDescriptor.id || "auto"/' $(clasp_dir)/node_modules/@formatjs/intl/lib/src/message.js
	@cd $(clasp_dir) && $(CURDIR)/$(bun_dir)/bin/bun build --compile src/index.ts --outfile $(CURDIR)/$@ >/dev/null
