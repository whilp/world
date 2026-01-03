modules += luaunit
luaunit_version := 3p/luaunit/version.lua
luaunit_srcs := luaunit.lua
luaunit_tests := $(wildcard 3p/luaunit/test_*.lua)
luaunit_staged := $(o)/$(luaunit_version).staged
luaunit_test_deps := luaunit
