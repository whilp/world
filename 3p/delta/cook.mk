delta_version := 3p/delta/version.lua
targets += o/%/delta/bin/delta
tests += o/%/delta/test.ok

o/%/delta/archive.tar.gz: $(delta_version) $(fetch)
	$(fetch) $(delta_version) $* $@

o/%/delta/staging/delta: $(delta_version) $(extract) o/%/delta/archive.tar.gz
	$(extract) $(delta_version) $* o/$*/delta/archive.tar.gz o/$*/delta/staging

o/%/delta/bin/delta: $(delta_version) $(install) o/%/delta/staging/delta
	$(install) $(delta_version) $* o/$*/delta bin o/$*/delta/staging/delta

o/%/delta/test.ok: 3p/delta/test.lua o/%/delta/bin/delta
	$< o/$*/delta && touch $@
