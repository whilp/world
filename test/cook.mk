modules += test
test_bin := o/test/bin
test_luatest := $(test_bin)/run-luatest.lua
test_files := $(wildcard test/*)
test_tests :=
test_deps := $(cosmic_files) $(luaunit_files)

export PATH := $(CURDIR)/$(test_bin):$(PATH)

$(test_luatest): test/run-luatest.lua $(test_deps)
	@mkdir -p $(@D)
	@cp $< $@
	@chmod a+x $@
