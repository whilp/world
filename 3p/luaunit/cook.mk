luaunit_version := 3p/luaunit/version.lua

o/any/luaunit/luaunit.lua: $(luaunit_version) $(fetch)
	$(fetch) $(luaunit_version) any $@
