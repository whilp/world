modules += appscript
appscript_files := $(o)/lib/appscript/appsscript.json
appscript_files += $(patsubst lib/appscript/%.gs,$(o)/lib/appscript/%.gs,$(wildcard lib/appscript/*.gs))
appscript_deps := clasp bun

# gs files to check
appscript_buns := $(wildcard lib/appscript/*.gs)

$(o)/lib/appscript/%.gs: lib/appscript/%.gs
	@mkdir -p $(@D)
	@cp $< $@

$(o)/lib/appscript/appsscript.json: lib/appscript/appsscript.json
	@mkdir -p $(@D)
	@cp $< $@
