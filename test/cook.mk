modules += test
test_luatest := $(o)/test/run-luatest.lua
test_files := $(test_luatest)
test_deps := $(cosmic_files) $(luaunit_files)

$(test_luatest): test/run-luatest.lua $(test_deps)
	@mkdir -p $(@D)
	@cp $< $@
	@chmod a+x $@
