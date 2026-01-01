luaunit_version := 3p/luaunit/version.lua
3p_lib_dirs += o/any/luaunit

o/any/luaunit/luaunit.lua: $(luaunit_version) $(fetch)
	$(fetch) $(luaunit_version) any $@
