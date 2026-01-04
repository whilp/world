modules += checker
checker_files := $(o)/lib/checker/common.lua
checker_tests := lib/checker/test_common.lua

$(o)/lib/checker/%.lua: lib/checker/%.lua
	@mkdir -p $(@D)
	@$(cp) $< $@
