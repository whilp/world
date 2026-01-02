tl_version := 3p/tl/version.lua
lua_libs += tl
3p_lib_dirs += o/%/tl/lib
libs += o/%/tl/lib/tl.lua
tl_bin := o/$(current_platform)/tl/bin/tl
bins += o/%/tl/bin/tl
tl_deps := \
	o/%/argparse/lib/argparse.lua \
	o/%/cosmos/bin/lua

o/any/3p/tl/test.lua.luatest.ok: o/$(current_platform)/tl/bin/tl
o/any/3p/tl/test.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/tl

o/%/tl/archive.tar.gz: $(tl_version) $(fetch)
	$(fetch) $(tl_version) $* $@

o/%/tl/staging/tl.lua: $(tl_version) $(extract) o/%/tl/archive.tar.gz
	$(extract) $(tl_version) $* o/$*/tl/archive.tar.gz o/$*/tl/staging

o/%/tl/lib/tl.lua: $(tl_version) $(install) o/%/tl/staging/tl.lua
	$(install) $(tl_version) $* o/$*/tl lib o/$*/tl/staging/tl.lua

o/%/tl/bin/tl: $(tl_version) $(install) o/%/tl/staging/tl.lua $(tl_deps)
	$(install) $(tl_version) $* o/$*/tl bin o/$*/tl/staging/tl
	cp o/$*/tl/staging/tl.lua o/$*/tl/bin/tl.lua
	chmod +x o/$*/tl/bin/tl
