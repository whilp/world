gh_version := 3p/gh/version.lua
bins += o/%/gh/bin/gh


o/%/gh/archive: $(gh_version) $(fetch)
	$(fetch) $(gh_version) $* $@

o/%/gh/staging/bin/gh: $(gh_version) $(extract) o/%/gh/archive
	$(extract) $(gh_version) $* o/$*/gh/archive o/$*/gh/staging

o/%/gh/bin/gh: $(gh_version) $(install) o/%/gh/staging/bin/gh
	$(install) $(gh_version) $* o/$*/gh bin o/$*/gh/staging/bin/gh
