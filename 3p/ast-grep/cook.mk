astgrep_version := 3p/ast-grep/version.lua
astgrep_config := sgconfig.yml
astgrep_rules := $(wildcard .ast-grep/rules/*.yml)
astgrep_bin := o/$(current_platform)/ast-grep/bin/ast-grep
astgrep_o := o/ast-grep
bins += o/%/ast-grep/bin/ast-grep

$(luatest_o)/3p/ast-grep/test.lua.ok: $(astgrep_bin)
$(luatest_o)/3p/ast-grep/test.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/ast-grep

$(luatest_o)/3p/ast-grep/test_rules.lua.ok: $(astgrep_bin) $(astgrep_config) $(astgrep_rules)
$(luatest_o)/3p/ast-grep/test_rules.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/ast-grep
$(luatest_o)/3p/ast-grep/test_rules.lua.ok: TEST_ARGS = $(CURDIR)/$(astgrep_config) $(CURDIR)/.ast-grep/rules

o/%/ast-grep/archive.zip: $(astgrep_version) $(fetch)
	$(fetch) $(astgrep_version) $* $@

o/%/ast-grep/staging/ast-grep: $(astgrep_version) $(extract) o/%/ast-grep/archive.zip
	$(extract) $(astgrep_version) $* o/$*/ast-grep/archive.zip o/$*/ast-grep/staging

o/%/ast-grep/bin/ast-grep: $(astgrep_version) $(install) o/%/ast-grep/staging/ast-grep
	$(install) $(astgrep_version) $* o/$*/ast-grep bin o/$*/ast-grep/staging/ast-grep
