# lib/cosmic - cosmopolitan lua utilities namespace

lib_lua_modules += cosmic
lib_dirs += o/any/cosmic/lib
lib_libs += o/any/cosmic/lib/cosmic/init.lua
lib_libs += o/any/cosmic/lib/cosmic/spawn.lua
lib_libs += o/any/cosmic/lib/cosmic/walk.lua
lib_libs += o/any/cosmic/lib/cosmic/help.lua

o/any/cosmic/lib/cosmic/%.lua: lib/cosmic/%.lua
	mkdir -p $(@D)
	cp $< $@

# tests depend on cosmic libs being built, plus spawn and walk
o/any/lib/cosmic/test_cosmic.lua.luatest.ok: o/any/cosmic/lib/cosmic/init.lua
o/any/lib/cosmic/test_cosmic.lua.luatest.ok: o/any/cosmic/lib/cosmic/spawn.lua
o/any/lib/cosmic/test_cosmic.lua.luatest.ok: o/any/cosmic/lib/cosmic/walk.lua
o/any/lib/cosmic/test_cosmic.lua.luatest.ok: o/any/cosmic/lib/cosmic/help.lua
o/any/lib/cosmic/test_cosmic.lua.luatest.ok: o/any/spawn/lib/spawn/init.lua
o/any/lib/cosmic/test_cosmic.lua.luatest.ok: o/any/walk/lib/walk/init.lua
