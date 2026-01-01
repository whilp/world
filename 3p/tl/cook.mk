tl_version := 3p/tl/version.lua
lua_libs += tl
3p_lib_dirs += o/%/tl/lib
libs += o/%/tl/lib/tl.lua
bins += o/%/tl/bin/tl
tests += o/%/tl/test.ok
tests += o/%/tl/test_typecheck_integration.ok
tl_deps := \
	o/%/argparse/lib/argparse.lua \
	o/%/cosmos/bin/lua

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

o/%/tl/test.ok: 3p/tl/test.lua o/%/tl/bin/tl $(runner)
	TEST_BIN_DIR=o/$*/tl $(runner) $< $@

o/%/tl/test_typecheck_integration.ok: 3p/tl/test_typecheck_integration.lua o/%/tl/bin/tl $(runner)
	TEST_BIN_DIR=o/$*/tl $(runner) $< $@
