gh_version := 3p/gh/version.lua
targets += o/%/gh/bin/gh
tests += o/%/gh/test.ok

o/%/gh/archive: $(gh_version) $(fetch)
	$(fetch) $(gh_version) $* $@

o/%/gh/staging/bin/gh: $(gh_version) $(extract) o/%/gh/archive
	$(extract) $(gh_version) $* o/$*/gh/archive o/$*/gh/staging

o/%/gh/bin/gh: $(gh_version) $(install) o/%/gh/staging/bin/gh
	$(install) $(gh_version) $* o/$*/gh/staging/bin/gh o/$*/gh

o/%/gh/test.ok: 3p/gh/test.lua o/%/gh/bin/gh
	$< o/$*/gh && touch $@
