lib_lua_modules += spawn
lib_dirs += lib/spawn
lib_tests += o/any/lib/spawn/test.ok

o/any/lib/spawn/test.ok: lib/spawn/test_spawn.lua lib/spawn/init.lua $(runner)
	$(runner) $< $@
