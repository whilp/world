luaunit_version := 3p/luaunit/version.lua
3p_lib_dirs += o/any/luaunit/lib

$(luatest_o)/3p/luaunit/test.lua.ok: o/any/luaunit/lib/luaunit.lua

o/any/luaunit/lib/luaunit.lua: $(luaunit_version) $(fetch)
	mkdir -p $(@D)
	$(fetch) $(luaunit_version) any $@
