luaunit_version := 3p/luaunit/version.lua
3p_lib_dirs += o/any/luaunit
tests += o/any/luaunit/test.ok

o/any/luaunit/luaunit.lua: $(luaunit_version) $(fetch)
	$(fetch) $(luaunit_version) any $@

o/any/luaunit/test.ok: 3p/luaunit/test.lua o/any/luaunit/luaunit.lua $(runner)
	$(runner) $< $@
