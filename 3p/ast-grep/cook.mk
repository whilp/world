astgrep_version := 3p/ast-grep/version.lua
bins += o/%/ast-grep/bin/ast-grep
tests += o/%/ast-grep/test.ok
tests += o/%/ast-grep/test_rules.ok

o/%/ast-grep/archive.zip: $(astgrep_version) $(fetch)
	$(fetch) $(astgrep_version) $* $@

o/%/ast-grep/staging/ast-grep: $(astgrep_version) $(extract) o/%/ast-grep/archive.zip
	$(extract) $(astgrep_version) $* o/$*/ast-grep/archive.zip o/$*/ast-grep/staging

o/%/ast-grep/bin/ast-grep: $(astgrep_version) $(install) o/%/ast-grep/staging/ast-grep
	$(install) $(astgrep_version) $* o/$*/ast-grep bin o/$*/ast-grep/staging/ast-grep

o/%/ast-grep/test.ok: 3p/ast-grep/test.lua o/%/ast-grep/bin/ast-grep $(runner)
	TEST_BIN_DIR=o/$*/ast-grep $(runner) $< $@

o/%/ast-grep/test_rules.ok: 3p/ast-grep/test_rules.lua o/%/ast-grep/bin/ast-grep sgconfig.yml $(runner)
	TEST_BIN_DIR=o/$*/ast-grep SGCONFIG=$(CURDIR)/sgconfig.yml RULES_DIR=$(CURDIR)/.ast-grep/rules $(runner) $< $@
