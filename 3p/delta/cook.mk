delta_version := 3p/delta/version.lua
bins += o/%/delta/bin/delta

o/luatest/3p/delta/test.lua.ok: o/$(current_platform)/delta/bin/delta
o/luatest/3p/delta/test.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/delta

o/%/delta/archive.tar.gz: $(delta_version) $(fetch)
	$(fetch) $(delta_version) $* $@

o/%/delta/staging/delta: $(delta_version) $(extract) o/%/delta/archive.tar.gz
	$(extract) $(delta_version) $* o/$*/delta/archive.tar.gz o/$*/delta/staging

o/%/delta/bin/delta: $(delta_version) $(install) o/%/delta/staging/delta
	$(install) $(delta_version) $* o/$*/delta bin o/$*/delta/staging/delta
