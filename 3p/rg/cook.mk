rg_version := 3p/rg/version.lua
bins += o/%/rg/bin/rg
tests += o/%/rg/test.ok

o/%/rg/archive.tar.gz: $(rg_version) $(fetch)
	$(fetch) $(rg_version) $* $@

o/%/rg/staging/rg: $(rg_version) $(extract) o/%/rg/archive.tar.gz
	$(extract) $(rg_version) $* o/$*/rg/archive.tar.gz o/$*/rg/staging

o/%/rg/bin/rg: $(rg_version) $(install) o/%/rg/staging/rg
	$(install) $(rg_version) $* o/$*/rg bin o/$*/rg/staging/rg

o/%/rg/test.ok: 3p/rg/test.lua o/%/rg/bin/rg
	$< o/$*/rg && touch $@
