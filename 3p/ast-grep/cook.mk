astgrep_version := 3p/ast-grep/version.lua
astgrep_rules := $(wildcard .ast-grep/rules/*.yml)
bins += o/%/ast-grep/bin/ast-grep

o/any/3p/ast-grep/test.lua.luatest.ok: o/$(current_platform)/ast-grep/bin/ast-grep
o/any/3p/ast-grep/test.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/ast-grep

o/any/3p/ast-grep/test_rules.lua.luatest.ok: o/$(current_platform)/ast-grep/bin/ast-grep sgconfig.yml $(astgrep_rules)
o/any/3p/ast-grep/test_rules.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/ast-grep
o/any/3p/ast-grep/test_rules.lua.luatest.ok: TEST_ARGS = $(CURDIR)/sgconfig.yml $(CURDIR)/.ast-grep/rules

o/%/ast-grep/archive.zip: $(astgrep_version) $(fetch)
	$(fetch) $(astgrep_version) $* $@

o/%/ast-grep/staging/ast-grep: $(astgrep_version) $(extract) o/%/ast-grep/archive.zip
	$(extract) $(astgrep_version) $* o/$*/ast-grep/archive.zip o/$*/ast-grep/staging

o/%/ast-grep/bin/ast-grep: $(astgrep_version) $(install) o/%/ast-grep/staging/ast-grep
	$(install) $(astgrep_version) $* o/$*/ast-grep bin o/$*/ast-grep/staging/ast-grep
