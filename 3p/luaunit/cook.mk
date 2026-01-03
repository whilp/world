modules += luaunit
luaunit_version := 3p/luaunit/version.lua
luaunit_srcs := luaunit.lua
luaunit_tests := $(wildcard 3p/luaunit/test_*.lua)

luaunit_staged := $(o)/3p/luaunit/version.lua.staged

$(patsubst %,$(o)/%.tested,$(luaunit_tests)): $(luaunit_staged)
