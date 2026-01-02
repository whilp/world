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

o/luatest/lib/build/test_luacheck.lua.ok: lib/build/luacheck.lua $(luacheck_bin)
o/luatest/lib/build/test_luacheck.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/luacheck
o/luatest/lib/build/test_luacheck.lua.ok: TEST_ARGS = $(CURDIR)/.luacheckrc

o/luatest/lib/build/test_ast_grep.lua.ok: lib/build/ast-grep.lua $(ast_grep)
o/luatest/lib/build/test_ast_grep.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/ast-grep
o/luatest/lib/build/test_ast_grep.lua.ok: TEST_ARGS = $(CURDIR)/sgconfig.yml $(CURDIR)/.ast-grep
