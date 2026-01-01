lib_lua_modules += spawn
lib_dirs += o/any/spawn/lib
lib_libs += o/any/spawn/lib/spawn/init.lua
lib_tests += o/%/spawn/test.ok

o/any/spawn/lib/spawn/init.lua: lib/spawn/init.lua
	mkdir -p $(@D)
	cp $< $@

o/%/spawn/test.ok: lib/spawn/test_spawn.lua o/any/spawn/lib/spawn/init.lua o/%/cosmos/bin/lua $(runner)
	TEST_BIN_DIR=o/$*/cosmos $(runner) $< $@
