# lib/build - build tools (fetch, extract, install)

lib_dirs += o/any/build/lib
lib_libs += o/any/build/lib/build/install.lua
lib_libs += o/any/build/lib/build/fetch.lua

o/any/build/lib/build/install.lua: lib/build/install.lua
	mkdir -p $(@D)
	cp $< $@

o/any/build/lib/build/fetch.lua: lib/build/fetch.lua
	mkdir -p $(@D)
	cp $< $@

o/any/lib/build/test_luacheck.luatest.ok: lib/build/test_luacheck.lua lib/build/luacheck.lua $(luacheck_bin) $(luatest_script) $(luaunit)
	TEST_BIN_DIR=o/$(current_platform)/luacheck $(luatest_runner) $< $@ $(CURDIR)/.luacheckrc

o/any/lib/build/test_ast_grep.luatest.ok: lib/build/test_ast_grep.lua lib/build/ast-grep.lua $(ast_grep) $(luatest_script) $(luaunit)
	TEST_BIN_DIR=o/$(current_platform)/ast-grep $(luatest_runner) $< $@ $(CURDIR)/sgconfig.yml $(CURDIR)/.ast-grep
