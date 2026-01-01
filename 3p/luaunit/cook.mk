luaunit_version := 3p/luaunit/version.lua
3p_lib_dirs += o/any/luaunit/lib
tests += o/any/luaunit/test.ok

o/any/luaunit/lib/luaunit.lua: $(luaunit_version) $(fetch)
	mkdir -p $(@D)
	$(fetch) $(luaunit_version) any $@

o/any/luaunit/test.ok: 3p/luaunit/test.lua o/any/luaunit/lib/luaunit.lua $(runner)
	$(runner) $< $@
