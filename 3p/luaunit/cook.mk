modules += luaunit
luaunit_version := 3p/luaunit/version.lua
luaunit_staged := o/$(luaunit_version).staged
luaunit_files := o/3p/luaunit/luaunit.lua
luaunit_tests :=

$(luaunit_files): $(luaunit_staged)
	@mkdir -p $(@D)
	cp $< $@
