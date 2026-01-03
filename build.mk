# build.mk - testing and linting infrastructure

# shared infrastructure
manifest_o := $(o)/manifest
manifest_git := $(manifest_o)/git.txt
manifest_luafiles := $(manifest_o)/lua-files.txt
manifest_luatests := $(manifest_o)/lua-tests.txt

manifest_script := lib/build/manifest.lua
script_deps := $(cosmic_lib)/cosmic/spawn.lua $(cosmic_lib)/cosmic/walk.lua

$(manifest_script): | $(lua_bin)

$(manifest_script): | $(script_deps)

$(manifest_git): .git/index
	@mkdir -p $(@D)
	git ls-files -z > $@

$(manifest_luafiles): $(manifest_git) $(manifest_script) $(script_deps) | $(lua_bin)
	@mkdir -p $(@D)
	$(lua_bin) $(manifest_script) find_lua_files > $@

$(manifest_luatests): $(manifest_git) $(manifest_script) $(script_deps) | $(lua_bin)
	@mkdir -p $(@D)
	$(lua_bin) $(manifest_script) find_lua_tests > $@

lua_files := $(shell cat $(manifest_luafiles) 2>/dev/null)
lua_files += lib/build/test_luafiles.lua
test_files := $(shell cat $(manifest_luatests) 2>/dev/null)
test_files += lib/build/test_luafiles.lua
version_files := $(shell git ls-files '**/version.lua' | grep -v '^lib/version\.lua$$')

# luatest - test runner
luatest_o := $(o)/luatest
luatest_script := lib/build/luatest.lua
luatest_runner = $(lua_bin) $(luatest_script)
luatest_files := $(patsubst %,$(luatest_o)/%.ok,$(test_files))

$(luatest_script): | $(lua_bin)
$(luatest_script): | $(script_deps)

luatest: $(luatest_files) ## Run tests incrementally on changed files

# lib/build test dependencies (here because they reference build.mk variables)
$(luatest_o)/lib/build/test_review.lua.ok: $(o_any)/build/lib/build/review.lua

$(luatest_o)/lib/build/test_luacheck.lua.ok: lib/build/luacheck.lua $(luacheck_bin)
$(luatest_o)/lib/build/test_luacheck.lua.ok: TEST_ENV = TEST_BIN_DIR=$(o_platform)/luacheck
$(luatest_o)/lib/build/test_luacheck.lua.ok: TEST_ARGS = $(CURDIR)/$(luacheck_config)

$(luatest_o)/lib/build/test_ast_grep.lua.ok: lib/build/ast-grep.lua $(astgrep_bin)
$(luatest_o)/lib/build/test_ast_grep.lua.ok: TEST_ENV = TEST_BIN_DIR=$(o_platform)/ast-grep
$(luatest_o)/lib/build/test_ast_grep.lua.ok: TEST_ARGS = $(CURDIR)/$(astgrep_config) $(CURDIR)/.ast-grep

$(luatest_o)/lib/build/test_pr.lua.ok: lib/build/pr.lua

$(luatest_o)/lib/build/test_luafiles.lua.ok: $(manifest_git) $(manifest_luafiles) $(manifest_luatests)
$(luatest_o)/lib/build/test_luafiles.lua.ok: TEST_ARGS = $(manifest_git) $(manifest_luafiles) $(manifest_luatests)

$(luatest_o)/%.ok: % $(luatest_script) $(luaunit) $(script_deps)
	$(TEST_ENV) $(luatest_runner) $< $@ $(TEST_ARGS)

luatest-report: $(luatest_files) $(script_deps) ## Run tests and show summary report
	@$(luatest_runner) report $(luatest_o)

# luacheck - lua linter
luacheck_o := $(o)/luacheck
luacheck_script := lib/build/luacheck.lua
luacheck_runner = $(lua_bin) $(luacheck_script)
luacheck_files := $(patsubst %,$(luacheck_o)/%.ok,$(lua_files))

$(luacheck_script): | $(lua_bin)
$(luacheck_script): | $(script_deps)

luacheck: $(luacheck_files) ## Run luacheck incrementally on changed files

$(luacheck_o)/%.ok: % $(luacheck_config) $(luacheck_script) $(luacheck_bin) $(script_deps)
	$(luacheck_runner) $< $@ $(luacheck_bin)

luacheck-report: $(luacheck_files) ## Run luacheck and show summary report
	@$(luacheck_runner) report $(luacheck_o)

# ast-grep - structural code search
astgrep_o := $(o)/ast-grep
ast_grep_script := lib/build/ast-grep.lua
ast_grep_runner = $(lua_bin) $(ast_grep_script)
ast_grep_files := $(patsubst %,$(astgrep_o)/%.ok,$(lua_files))

$(ast_grep_script): | $(lua_bin)
$(ast_grep_script): | $(script_deps)

ast-grep: $(ast_grep_files) ## Run ast-grep incrementally on changed files

$(astgrep_o)/%.ok: % $(astgrep_config) $(ast_grep_script) $(astgrep_bin) $(script_deps)
	$(ast_grep_runner) $< $@ $(astgrep_bin)

ast-grep-report: $(ast_grep_files) ## Run ast-grep and show summary report
	@$(ast_grep_runner) report $(astgrep_o)

# teal - gradual type checker
tl_o := $(o)/teal
teal_script := lib/build/teal.lua
teal_runner = $(lua_bin) $(teal_script)
teal_files := $(patsubst %,$(tl_o)/%.ok,$(lua_files))

$(teal_script): | $(lua_bin)
$(teal_script): | $(script_deps)

teal: $(teal_files) ## Run teal incrementally on changed files

$(tl_o)/%.ok: % $(teal_script) $(tl_bin) $(lua_dist) $(script_deps)
	$(teal_runner) $< $@ $(tl_bin) $(lua_dist) || true

teal-report: $(teal_files) ## Run teal and show summary report
	# TODO: remove || true once all files pass
	@$(teal_runner) report $(tl_o) || true

# latest - version checker
latest_script := lib/build/latest.lua
latest = $(lua_bin) $(latest_script)
latest_runner = $(lua_bin) $(latest_script)
latest_files := $(patsubst %,o/any/%.latest.ok,$(version_files))

$(latest_script): | $(lua_bin)

latest: $(latest_files) ## Check for latest versions incrementally on changed files

o/any/%.latest.ok: % $(latest_script)
	$(latest) $< $@

latest-report: $(latest_files) ## Check latest versions and show summary report
	@$(latest_runner) report o/any

# build scripts - shared utilities
fetch_script := lib/build/fetch.lua
extract_script := lib/build/extract.lua
install_script := lib/build/install.lua

fetch = $(lua_bin) $(fetch_script)
extract = $(lua_bin) $(extract_script)
install = $(lua_bin) $(install_script)

$(fetch_script) $(extract_script) $(install_script): | $(lua_bin)
$(extract_script): | $(cosmic_lib)/cosmic/spawn.lua
