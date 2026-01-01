lib_lua_modules += spawn
lib_dirs += o/any/spawn/lib
lib_libs += o/any/spawn/lib/spawn/init.lua
lib_tests += o/%/spawn/test.ok
luacheck_files += o/any/spawn/init.luacheck.ok
luacheck_files += o/any/spawn/test_spawn.luacheck.ok

o/any/spawn/lib/spawn/init.lua: lib/spawn/init.lua
	mkdir -p $(@D)
	cp $< $@

o/%/spawn/test.ok: lib/spawn/test_spawn.lua o/any/spawn/lib/spawn/init.lua o/%/cosmos/bin/lua $(runner)
	TEST_BIN_DIR=o/$*/cosmos $(runner) $< $@

o/any/spawn/%.luacheck.ok: lib/spawn/%.lua .luacheckrc $(luacheck_script) $(luacheck_bin)
	$(luacheck_runner) $< $@ $(luacheck_bin)
