ruff_version := 3p/ruff/version.lua
bins += o/%/ruff/bin/ruff

$(luatest_o)/3p/ruff/test.lua.ok: o/$(current_platform)/ruff/bin/ruff
$(luatest_o)/3p/ruff/test.lua.ok: TEST_ENV = TEST_BIN_DIR=$(o_platform)/ruff

o/%/ruff/archive.tar.gz: $(ruff_version) $(fetch)
	$(fetch) $(ruff_version) $* $@

o/%/ruff/staging/ruff: $(ruff_version) $(extract) o/%/ruff/archive.tar.gz
	$(extract) $(ruff_version) $* o/$*/ruff/archive.tar.gz o/$*/ruff/staging

o/%/ruff/bin/ruff: $(ruff_version) $(install) o/%/ruff/staging/ruff
	$(install) $(ruff_version) $* o/$*/ruff bin o/$*/ruff/staging/ruff
