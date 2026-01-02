luaunit_version := 3p/luaunit/version.lua
luaunit := $(o_any)/luaunit/lib/luaunit.lua
3p_lib_dirs += $(o_any)/luaunit/lib

$(luatest_o)/3p/luaunit/test.lua.ok: $(luaunit)

$(luaunit): $(luaunit_version) $(fetch)
	mkdir -p $(@D)
	$(fetch) $(luaunit_version) any $@
