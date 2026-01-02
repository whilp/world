uv_version := 3p/uv/version.lua
bins += o/%/uv/bin/uv

o/luatest/3p/uv/test.lua.ok: o/$(current_platform)/uv/bin/uv
o/luatest/3p/uv/test.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/uv

o/%/uv/archive.tar.gz: $(uv_version) $(fetch)
	$(fetch) $(uv_version) $* $@

o/%/uv/staging/uv: $(uv_version) $(extract) o/%/uv/archive.tar.gz
	$(extract) $(uv_version) $* o/$*/uv/archive.tar.gz o/$*/uv/staging

o/%/uv/bin/uv: $(uv_version) $(install) o/%/uv/staging/uv
	$(install) $(uv_version) $* o/$*/uv bin o/$*/uv/staging/uv
