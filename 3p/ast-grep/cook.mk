astgrep_version := 3p/ast-grep/version.lua
targets += o/%/ast-grep/bin/sg
tests += o/%/ast-grep/test.ok

o/%/ast-grep/archive.zip: $(astgrep_version) $(fetch)
	$(fetch) $(astgrep_version) $* $@

o/%/ast-grep/staging/sg: $(astgrep_version) $(extract) o/%/ast-grep/archive.zip
	$(extract) $(astgrep_version) $* o/$*/ast-grep/archive.zip o/$*/ast-grep/staging

o/%/ast-grep/bin/sg: $(astgrep_version) $(install) o/%/ast-grep/staging/sg
	$(install) $(astgrep_version) $* o/$*/ast-grep bin o/$*/ast-grep/staging/sg

o/%/ast-grep/test.ok: 3p/ast-grep/test.lua o/%/ast-grep/bin/sg
	$< o/$*/ast-grep && touch $@
