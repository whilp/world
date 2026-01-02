astgrep_version := 3p/ast-grep/version.lua
astgrep_rules := $(wildcard .ast-grep/rules/*.yml)
astgrep_bin := o/$(current_platform)/ast-grep/bin/ast-grep
bins += o/%/ast-grep/bin/ast-grep

o/luatest/3p/ast-grep/test.lua.ok: o/$(current_platform)/ast-grep/bin/ast-grep
o/luatest/3p/ast-grep/test.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/ast-grep

o/luatest/3p/ast-grep/test_rules.lua.ok: o/$(current_platform)/ast-grep/bin/ast-grep sgconfig.yml $(astgrep_rules)
o/luatest/3p/ast-grep/test_rules.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/ast-grep
o/luatest/3p/ast-grep/test_rules.lua.ok: TEST_ARGS = $(CURDIR)/sgconfig.yml $(CURDIR)/.ast-grep/rules

o/%/ast-grep/archive.zip: $(astgrep_version) $(fetch)
	$(fetch) $(astgrep_version) $* $@

o/%/ast-grep/staging/ast-grep: $(astgrep_version) $(extract) o/%/ast-grep/archive.zip
	$(extract) $(astgrep_version) $* o/$*/ast-grep/archive.zip o/$*/ast-grep/staging

o/%/ast-grep/bin/ast-grep: $(astgrep_version) $(install) o/%/ast-grep/staging/ast-grep
	$(install) $(astgrep_version) $* o/$*/ast-grep bin o/$*/ast-grep/staging/ast-grep
