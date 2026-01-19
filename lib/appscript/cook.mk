modules += appscript
appscript_files := $(o)/lib/appscript/appsscript.json
appscript_files += $(patsubst lib/appscript/%.gs,$(o)/lib/appscript/%.gs,$(wildcard lib/appscript/*.gs))
appscript_tests := $(wildcard lib/appscript/*.test.js)
appscript_deps := clasp bun

# gs files to check
appscript_buns := $(wildcard lib/appscript/*.gs)

appscript_runner := lib/appscript/run-test.js

$(o)/lib/appscript/%.gs: lib/appscript/%.gs
	@mkdir -p $(@D)
	@cp $< $@

$(o)/lib/appscript/appsscript.json: lib/appscript/appsscript.json
	@mkdir -p $(@D)
	@cp $< $@

.PHONY: appscript-push
## Push appscript files to Google Apps Script
appscript-push: $(patsubst %,$(o)/%.bun.ok,$(appscript_buns)) $$(clasp_bin)
	@cd lib/appscript && $(CURDIR)/$(clasp_bin) push --force

# JavaScript test rule for appscript
$(o)/lib/appscript/%.test.js.test.ok: lib/appscript/%.test.js $(appscript_runner) $(appscript_files) $$(bun_staged)
	@mkdir -p $(@D)
	-@$(bun_dir)/bin/bun run $(appscript_runner) $< > $@
