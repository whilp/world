# lib/build - build tools (fetch, extract, install)

luatest_o := $(o)/luatest

lib_dirs += $(o_any)/build/lib
lib_libs += $(o_any)/build/lib/build/install.lua
lib_libs += $(o_any)/build/lib/build/fetch.lua
lib_libs += $(o_any)/build/lib/build/review.lua

$(o_any)/build/lib/build/%.lua: lib/build/%.lua
	mkdir -p $(@D)
	cp $< $@

$(luatest_o)/lib/build/test_review.lua.ok: $(o_any)/build/lib/build/review.lua

$(luatest_o)/lib/build/test_luacheck.lua.ok: lib/build/luacheck.lua $(luacheck_bin)
$(luatest_o)/lib/build/test_luacheck.lua.ok: TEST_ENV = TEST_BIN_DIR=$(o_platform)/luacheck
$(luatest_o)/lib/build/test_luacheck.lua.ok: TEST_ARGS = $(CURDIR)/$(luacheck_config)

$(luatest_o)/lib/build/test_ast_grep.lua.ok: lib/build/ast-grep.lua $(astgrep_bin)
$(luatest_o)/lib/build/test_ast_grep.lua.ok: TEST_ENV = TEST_BIN_DIR=$(o_platform)/ast-grep
$(luatest_o)/lib/build/test_ast_grep.lua.ok: TEST_ARGS = $(CURDIR)/$(astgrep_config) $(CURDIR)/.ast-grep

update-pr: $(lua_bin) ## Update PR title/description from .github/pr.md
	@test -f .github/pr.md && $(lua_bin) lib/build/pr.lua || true

.PHONY: update-pr
