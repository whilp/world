superhtml_version := 3p/superhtml/version.lua
targets += o/%/superhtml/bin/superhtml
tests += o/%/superhtml/test.ok

o/%/superhtml/archive.tar.gz: $(superhtml_version) $(fetch)
	$(fetch) $(superhtml_version) $* $@

o/%/superhtml/staging/superhtml: $(superhtml_version) $(extract) o/%/superhtml/archive.tar.gz
	$(extract) $(superhtml_version) $* o/$*/superhtml/archive.tar.gz o/$*/superhtml/staging

o/%/superhtml/bin/superhtml: $(superhtml_version) $(install) o/%/superhtml/staging/superhtml
	$(install) $(superhtml_version) $* o/$*/superhtml bin o/$*/superhtml/staging/superhtml

o/%/superhtml/test.ok: 3p/superhtml/test.lua o/%/superhtml/bin/superhtml
	$< o/$*/superhtml && touch $@
