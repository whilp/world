comrak_version := 3p/comrak/version.lua
bins += o/%/comrak/bin/comrak
tests += o/%/comrak/test.ok

o/%/comrak/download: $(comrak_version) $(fetch)
	$(fetch) $(comrak_version) $* $@

o/%/comrak/bin/comrak: $(comrak_version) $(install) o/%/comrak/download
	$(install) $(comrak_version) $* o/$*/comrak bin o/$*/comrak/download

o/%/comrak/test.ok: 3p/comrak/test.lua o/%/comrak/bin/comrak
	$< o/$*/comrak && touch $@
