argparse_version := 3p/argparse/version.lua
lua_libs += argparse
3p_lib_dirs += o/%/argparse/lib
libs += o/%/argparse/lib/argparse.lua

o/luatest/3p/argparse/test.lua.ok: o/$(current_platform)/argparse/lib/argparse.lua

o/%/argparse/archive.tar.gz: $(argparse_version) $(fetch)
	$(fetch) $(argparse_version) $* $@

o/%/argparse/staging/src/argparse.lua: $(argparse_version) $(extract) o/%/argparse/archive.tar.gz
	$(extract) $(argparse_version) $* o/$*/argparse/archive.tar.gz o/$*/argparse/staging

o/%/argparse/lib/argparse.lua: $(argparse_version) $(install) o/%/argparse/staging/src/argparse.lua
	$(install) $(argparse_version) $* o/$*/argparse lib o/$*/argparse/staging/src/argparse.lua
