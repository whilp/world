luacheck_version := 3p/luacheck/version.lua
lua_libs += luacheck
3p_lib_dirs += o/%/luacheck/lib
libs += o/%/luacheck/lib/luacheck/main.lua
bins += o/%/luacheck/bin/luacheck
luacheck_deps := \
	o/%/argparse/lib/argparse.lua \
	o/%/lfs/lib/lfs.lua \
	o/%/cosmos/bin/lua \
	3p/luacheck/luacheck

o/any/3p/luacheck/test.lua.luatest.ok: o/$(current_platform)/luacheck/bin/luacheck
o/any/3p/luacheck/test.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/luacheck

o/%/luacheck/archive.tar.gz: $(luacheck_version) $(fetch)
	$(fetch) $(luacheck_version) $* $@

o/%/luacheck/staging/src/luacheck/main.lua: $(luacheck_version) $(extract) o/%/luacheck/archive.tar.gz
	$(extract) $(luacheck_version) $* o/$*/luacheck/archive.tar.gz o/$*/luacheck/staging

o/%/luacheck/lib/luacheck/main.lua: $(luacheck_version) $(install) o/%/luacheck/staging/src/luacheck/main.lua
	$(install) $(luacheck_version) $* o/$*/luacheck lib o/$*/luacheck/staging/src

o/%/luacheck/bin/luacheck: $(luacheck_version) $(install) o/%/luacheck/lib/luacheck/main.lua $(luacheck_deps)
	$(install) $(luacheck_version) $* o/$*/luacheck bin 3p/luacheck/luacheck
	chmod +x o/$*/luacheck/bin/luacheck
