luacheck_version := 3p/luacheck/version.lua
lua_libs += luacheck
targets += o/%/luacheck/lib/luacheck/main.lua o/%/luacheck/bin/luacheck
tests += o/%/luacheck/test.ok

o/%/luacheck/archive.tar.gz: $(luacheck_version) $(fetch)
	$(fetch) $(luacheck_version) $* $@

o/%/luacheck/staging/src/luacheck/main.lua: $(luacheck_version) $(extract) o/%/luacheck/archive.tar.gz
	$(extract) $(luacheck_version) $* o/$*/luacheck/archive.tar.gz o/$*/luacheck/staging

o/%/luacheck/lib/luacheck/main.lua: $(luacheck_version) $(install) o/%/luacheck/staging/src/luacheck/main.lua
	$(install) $(luacheck_version) $* o/$*/luacheck lib o/$*/luacheck/staging/src/luacheck

o/%/luacheck/bin/luacheck: $(luacheck_version) $(install) o/%/luacheck/lib/luacheck/main.lua o/%/argparse/lib/argparse.lua o/%/lfs/lib/lfs.lua o/%/cosmos/bin/lua 3p/luacheck/luacheck
	$(install) $(luacheck_version) $* o/$*/luacheck bin 3p/luacheck/luacheck
	chmod +x o/$*/luacheck/bin/luacheck

o/%/luacheck/test.ok: 3p/luacheck/test.lua o/%/luacheck/bin/luacheck
	TEST_BIN_DIR=o/$*/luacheck $(runner) $< $@
