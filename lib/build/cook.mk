# lib/build - build tools (fetch, extract, install)

luatest_o := o/luatest

lib_dirs += o/any/build/lib
lib_libs += o/any/build/lib/build/install.lua
lib_libs += o/any/build/lib/build/fetch.lua

o/any/build/lib/build/install.lua: lib/build/install.lua
	mkdir -p $(@D)
	cp $< $@

o/any/build/lib/build/fetch.lua: lib/build/fetch.lua
	mkdir -p $(@D)
	cp $< $@

$(luatest_o)/lib/build/test_luacheck.lua.ok: lib/build/luacheck.lua $(luacheck_bin)
$(luatest_o)/lib/build/test_luacheck.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/luacheck
$(luatest_o)/lib/build/test_luacheck.lua.ok: TEST_ARGS = $(CURDIR)/$(luacheck_config)

$(luatest_o)/lib/build/test_ast_grep.lua.ok: lib/build/ast-grep.lua $(astgrep_bin)
$(luatest_o)/lib/build/test_ast_grep.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/ast-grep
$(luatest_o)/lib/build/test_ast_grep.lua.ok: TEST_ARGS = $(CURDIR)/$(astgrep_config) $(CURDIR)/.ast-grep
