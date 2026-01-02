# lib/build - build tools (fetch, extract, install)

lib_dirs += o/any/build/lib
lib_libs += o/any/build/lib/build/install.lua
lib_libs += o/any/build/lib/build/fetch.lua
lib_libs += o/any/build/lib/build/review.lua

o/any/build/lib/build/%.lua: lib/build/%.lua
	mkdir -p $(@D)
	cp $< $@

o/any/lib/build/test_review.lua.luatest.ok: o/any/build/lib/build/review.lua

o/any/lib/build/test_luacheck.lua.luatest.ok: lib/build/luacheck.lua $(luacheck_bin)
o/any/lib/build/test_luacheck.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/luacheck
o/any/lib/build/test_luacheck.lua.luatest.ok: TEST_ARGS = $(CURDIR)/.luacheckrc

o/any/lib/build/test_ast_grep.lua.luatest.ok: lib/build/ast-grep.lua $(ast_grep)
o/any/lib/build/test_ast_grep.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/ast-grep
o/any/lib/build/test_ast_grep.lua.luatest.ok: TEST_ARGS = $(CURDIR)/sgconfig.yml $(CURDIR)/.ast-grep
