sqruff_version := 3p/sqruff/version.lua
bins += o/%/sqruff/bin/sqruff

o/any/3p/sqruff/test.lua.luatest.ok: o/$(current_platform)/sqruff/bin/sqruff
o/any/3p/sqruff/test.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/sqruff

o/%/sqruff/archive.tar.gz: $(sqruff_version) $(fetch)
	$(fetch) $(sqruff_version) $* $@

o/%/sqruff/staging/sqruff: $(sqruff_version) $(extract) o/%/sqruff/archive.tar.gz
	$(extract) $(sqruff_version) $* o/$*/sqruff/archive.tar.gz o/$*/sqruff/staging

o/%/sqruff/bin/sqruff: $(sqruff_version) $(install) o/%/sqruff/staging/sqruff
	$(install) $(sqruff_version) $* o/$*/sqruff bin o/$*/sqruff/staging/sqruff
