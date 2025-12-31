luaunit_version := 3p/luaunit/version.lua

lib/luaunit.lua: $(luaunit_version) $(fetch)
	$(fetch) $(luaunit_version) any $@
