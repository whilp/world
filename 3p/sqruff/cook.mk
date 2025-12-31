sqruff_version := 3p/sqruff/version.lua
targets += o/%/sqruff/bin/sqruff

o/%/sqruff/archive.tar.gz: $(sqruff_version) $(fetch)
	$(fetch) $(sqruff_version) $* $@

o/%/sqruff/staging/sqruff: $(sqruff_version) $(extract) o/%/sqruff/archive.tar.gz
	$(extract) $(sqruff_version) $* o/$*/sqruff/archive.tar.gz o/$*/sqruff/staging

o/%/sqruff/bin/sqruff: $(sqruff_version) $(install) o/%/sqruff/staging/sqruff
	$(install) $(sqruff_version) $* o/$*/sqruff/staging/sqruff o/$*/sqruff
