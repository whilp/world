modules += luaunit
luaunit_version := 3p/luaunit/version.lua
luaunit_srcs := luaunit.lua
luaunit_tests := $(wildcard 3p/luaunit/test_*.lua)

# staged output location
luaunit_staged := $(o)/$(luaunit_version).staged

# explicit targets for test deps and STAGED_DIR
luaunit_tested := $(patsubst %,$(o)/%.tested,$(luaunit_tests))
$(luaunit_tested): STAGED_DIR := $(luaunit_staged)
$(luaunit_tested): $(luaunit_staged)
