marksman_version := 3p/marksman/version.lua
bins += o/%/marksman/bin/marksman

$(luatest_o)/3p/marksman/test.lua.ok: o/$(current_platform)/marksman/bin/marksman
$(luatest_o)/3p/marksman/test.lua.ok: TEST_ENV = TEST_BIN_DIR=$(o_platform)/marksman

o/%/marksman/download: $(marksman_version) $(fetch)
	$(fetch) $(marksman_version) $* $@

o/%/marksman/bin/marksman: $(marksman_version) $(install) o/%/marksman/download
	$(install) $(marksman_version) $* o/$*/marksman bin o/$*/marksman/download
