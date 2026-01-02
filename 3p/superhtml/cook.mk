superhtml_version := 3p/superhtml/version.lua
bins += o/%/superhtml/bin/superhtml

$(luatest_o)/3p/superhtml/test.lua.ok: o/$(current_platform)/superhtml/bin/superhtml
$(luatest_o)/3p/superhtml/test.lua.ok: TEST_ENV = TEST_BIN_DIR=$(o_platform)/superhtml

o/%/superhtml/archive.tar.gz: $(superhtml_version) $(fetch)
	$(fetch) $(superhtml_version) $* $@

o/%/superhtml/staging/superhtml: $(superhtml_version) $(extract) o/%/superhtml/archive.tar.gz
	$(extract) $(superhtml_version) $* o/$*/superhtml/archive.tar.gz o/$*/superhtml/staging

o/%/superhtml/bin/superhtml: $(superhtml_version) $(install) o/%/superhtml/staging/superhtml
	$(install) $(superhtml_version) $* o/$*/superhtml bin o/$*/superhtml/staging/superhtml
