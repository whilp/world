lib_lua_modules += spawn
lib_dirs += o/any/spawn/lib
spawn_lua := $(wildcard lib/spawn/*.lua)
spawn_lib := $(filter-out lib/spawn/test%.lua,$(spawn_lua))
spawn_test := $(filter lib/spawn/test%.lua,$(spawn_lua))
lib_libs += $(patsubst lib/%,o/any/lib/%,$(spawn_lib))
lib_tests += $(patsubst lib/spawn/%.lua,o/%/spawn/%.ok,$(spawn_test))
luacheck_files += $(patsubst lib/%.lua,o/any/%.luacheck.ok,$(spawn_lua))

o/any/spawn/lib/spawn/init.lua: lib/spawn/init.lua
	mkdir -p $(@D)
	cp $< $@

o/%/spawn/test.ok: lib/spawn/test_spawn.lua o/any/spawn/lib/spawn/init.lua o/%/cosmos/bin/lua $(runner)
	TEST_BIN_DIR=o/$*/cosmos $(runner) $< $@
