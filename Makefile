modules :=
o := o

uname_s := $(shell uname -s)
uname_m := $(shell uname -m)
os := $(if $(filter Darwin,$(uname_s)),darwin,linux)
arch := $(subst aarch64,arm64,$(uname_m))
platforms := darwin-arm64 linux-arm64 linux-x86_64
platform := $(os)-$(arch)

include bootstrap/cook.mk
include 3p/luaunit/cook.mk
#include lib/cosmic/cook.mk
include build/cook.mk
include test/cook.mk

include cook.mk

all_files := $(foreach x,$(modules),$($(x)_files))
all_files += $(foreach x,$(modules),$(addprefix $(o)/$(x)/,$($(x)_srcs)))
all_tests := $(foreach x,$(modules),$($(x)_tests))
all_versions := $(foreach x,$(modules),$($(x)_version))

# expand module deps: M_files depends on each dep's _files
$(foreach m,$(modules),\
  $(foreach d,$($(m)_deps),\
    $(eval $($(m)_files): $($(d)_files))))

# expand test deps: M's tested targets depend on each dep's _staged, get STAGED_DIR
$(foreach m,$(modules),\
  $(foreach d,$($(m)_test_deps),\
    $(eval $(patsubst %,$(o)/%.tested,$($(m)_tests)): STAGED_DIR := $($(d)_staged))\
    $(eval $(patsubst %,$(o)/%.tested,$($(m)_tests)): $($(d)_staged))))

cp := cp -p

$(o)/%: %
	@mkdir -p $(@D)
	@$(cp) $< $@

.PHONY: fetched
all_fetched := $(patsubst %,o/%.fetched,$(all_versions))
fetched: $(all_fetched)
$(o)/%.fetched: % $(build_files) | $(bootstrap_cosmic)
	$(build_fetch) $< $(platform) $@

.PHONY: staged
all_staged := $(patsubst %,o/%.staged,$(all_versions))
staged: $(all_staged)
$(o)/%.staged: $(o)/%.fetched
	$(build_stage) $* $< $@

.PHONY: test
all_tests := $(foreach x,$(modules),$($(x)_tests))
all_tested := $(patsubst %,o/%.tested,$(all_tests))
test: $(all_tested)
$(o)/%.tested: % $(test_files) $(test_deps)
	@STAGED_DIR="$(STAGED_DIR)" $< $@

.PHONY: clean
clean:
	rm -rf $(o)

debug-modules:
	@echo $(modules)

# o := o
# o_platform := $(o)/$(current_platform)
# o_any := $(o)/any
# manifest_o := $(o)/manifest
# manifest_git := $(manifest_o)/git.txt
# manifest_luafiles := $(manifest_o)/lua-files.txt
# manifest_luatests := $(manifest_o)/lua-tests.txt
#
# export PATH := $(CURDIR)/$(o_platform)/cosmos/bin:$(CURDIR)/$(o_any)/lua/bin:$(PATH)
#
# lua_bin := $(o_any)/lua/bin/lua
#
# # Script paths (for dependencies)
# fetch_script := lib/build/fetch.lua
# extract_script := lib/build/extract.lua
# install_script := lib/build/install.lua
# latest_script := lib/build/latest.lua
# luatest_script := lib/build/luatest.lua
# luacheck_script := lib/build/luacheck.lua
# ast_grep_script := lib/build/ast-grep.lua
# teal_script := lib/build/teal.lua
# manifest_script := lib/build/manifest.lua
#
# # Commands (invoke lua explicitly to avoid APE "Text file busy" errors)
# fetch = $(lua_bin) $(fetch_script)
# extract = $(lua_bin) $(extract_script)
# install = $(lua_bin) $(install_script)
# latest = $(lua_bin) $(latest_script)
# latest_runner = $(lua_bin) $(latest_script)
# runner = $(lua_bin) $(luatest_script)
# luatest_runner = $(lua_bin) $(luatest_script)
# luacheck_runner = $(lua_bin) $(luacheck_script)
# ast_grep_runner = $(lua_bin) $(ast_grep_script)
# teal_runner = $(lua_bin) $(teal_script)
#
# $(fetch_script) $(extract_script) $(install_script) $(latest_script) $(luatest_script) $(luacheck_script) $(ast_grep_script) $(teal_script) $(manifest_script): | $(lua_bin)
# cosmo := whilp/cosmopolitan
# release ?= latest
#
# include lib/cook.mk
# include 3p/cook.mk
#
# # Build LUA_PATH from lib_dirs and 3p_lib_dirs
# null :=
# space := $(null) $(null)
# lib_paths := $(subst $(space),,$(foreach dir,$(lib_dirs),$(CURDIR)/$(dir)/?.lua;$(CURDIR)/$(dir)/?/init.lua;))
# 3p_lib_paths := $(subst $(space),,$(foreach dir,$(subst %,$(current_platform),$(3p_lib_dirs)),$(CURDIR)/$(dir)/?.lua;$(CURDIR)/$(dir)/?/init.lua;))
# export LUA_PATH := $(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;$(lib_paths)$(3p_lib_paths)/zip/.lua/?.lua;/zip/.lua/?/init.lua
#
# # Script dependencies from cosmic module
# script_deps := $(cosmic_lib)/cosmic/spawn.lua $(cosmic_lib)/cosmic/walk.lua
#
# # Build scripts that require runtime dependencies
# $(extract_script): | $(cosmic_lib)/cosmic/spawn.lua
# $(luatest_script) $(luacheck_script) $(ast_grep_script) $(teal_script) $(manifest_script): | $(script_deps)
#
# # Manifest files
# $(manifest_git): .git/index
# 	@mkdir -p $(@D)
# 	git ls-files -z > $@
#
# $(manifest_luafiles): $(manifest_git) $(manifest_script) $(script_deps) | $(lua_bin)
# 	@mkdir -p $(@D)
# 	$(lua_bin) $(manifest_script) find_lua_files > $@
#
# $(manifest_luatests): $(manifest_git) $(manifest_script) $(script_deps) | $(lua_bin)
# 	@mkdir -p $(@D)
# 	$(lua_bin) $(manifest_script) find_lua_tests > $@
#
# lua_files := $(shell cat $(manifest_luafiles) 2>/dev/null)
# lua_files += lib/build/test_luafiles.lua
# test_files := $(shell cat $(manifest_luatests) 2>/dev/null)
# test_files += lib/build/test_luafiles.lua
# version_files := $(shell git ls-files '**/version.lua' | grep -v '^lib/version\.lua$$')
# luatest_files := $(patsubst %,$(luatest_o)/%.ok,$(test_files))
# luacheck_files := $(patsubst %,$(luacheck_o)/%.ok,$(lua_files))
# latest_files := $(patsubst %,o/any/%.latest.ok,$(version_files))
#
# luatest: $(luatest_files) ## Run tests incrementally on changed files
#
# $(luatest_o)/%.ok: % $(luatest_script) $(luaunit) $(script_deps)
# 	$(TEST_ENV) $(luatest_runner) $< $@ $(TEST_ARGS)
#
# # Pattern rules for 3p test dependencies
# define 3p_test_rule
# $(luatest_o)/3p/$(1)/test.lua.ok: o/$$(current_platform)/$(1)/bin/$(1)
# $(luatest_o)/3p/$(1)/test.lua.ok: TEST_ENV = TEST_BIN_DIR=$$(o_platform)/$(1)
# endef
#
# $(foreach mod,duckdb ruff gh superhtml delta biome comrak nvim uv tree-sitter stylua sqruff marksman rg shfmt,$(eval $(call 3p_test_rule,$(mod))))
#
# luatest-report: $(luatest_files) $(script_deps) ## Run tests and show summary report
# 	@$(luatest_runner) report $(luatest_o)
#
# luacheck: $(luacheck_files) ## Run luacheck incrementally on changed files
#
# $(luacheck_o)/%.ok: % $(luacheck_config) $(luacheck_script) $(luacheck_bin) $(script_deps)
# 	$(luacheck_runner) $< $@ $(luacheck_bin)
#
# luacheck-report: $(luacheck_files) ## Run luacheck and show summary report
# 	@$(luacheck_runner) report $(luacheck_o)
#
# ast_grep_files := $(patsubst %,$(astgrep_o)/%.ok,$(lua_files))
#
# ast-grep: $(ast_grep_files) ## Run ast-grep incrementally on changed files
#
# $(astgrep_o)/%.ok: % $(astgrep_config) $(ast_grep_script) $(astgrep_bin) $(script_deps)
# 	$(ast_grep_runner) $< $@ $(astgrep_bin)
#
# ast-grep-report: $(ast_grep_files) ## Run ast-grep and show summary report
# 	@$(ast_grep_runner) report $(astgrep_o)
#
# teal_files := $(patsubst %,$(tl_o)/%.ok,$(lua_files))
#
# teal: $(teal_files) ## Run teal incrementally on changed files
#
# $(tl_o)/%.ok: % $(teal_script) $(tl_bin) $(lua_dist) $(script_deps)
# 	$(teal_runner) $< $@ $(tl_bin) $(lua_dist) || true
#
# teal-report: $(teal_files) ## Run teal and show summary report
# 	# TODO: remove || true once all files pass
# 	@$(teal_runner) report $(tl_o) || true
#
# latest: $(latest_files) ## Check for latest versions incrementally on changed files
#
# o/any/%.latest.ok: % $(latest_script)
# 	$(latest) $< $@
#
# latest-report: $(latest_files) ## Check latest versions and show summary report
# 	@$(latest_runner) report o/any
#
# bootstrap: $(lua_bin)
# 	@[ -n "$$CLAUDE_ENV_FILE" ] && echo "PATH=$(dir $(lua_bin)):\$$PATH" >> "$$CLAUDE_ENV_FILE"; true
#
# cosmic_release := home-2026-01-02-75caba6
#
# $(lua_bin):
# 	@mkdir -p $(@D)
# 	curl -sL -o $@ "https://github.com/whilp/world/releases/download/$(cosmic_release)/cosmic-lua"
# 	@chmod +x $@
#
# cosmos: o/$(current_platform)/cosmos/bin/lua
# lua: o/$(current_platform)/lua/bin/lua.dist
#
# check: $(ast_grep_files) $(luacheck_files) $(teal_files) ## Run ast-grep, luacheck, and teal
# 	@$(ast_grep_runner) report o/ast-grep
# 	@echo ""
# 	@$(luacheck_runner) report o/luacheck
# 	@echo ""
# 	# TODO: remove || true once all files pass teal
# 	@$(teal_runner) report o/teal || true
#
# test: $(luatest_files)
# 	@echo "All tests passed"
#
# clean:
# 	rm -rf o
#
# .PHONY: bootstrap clean cosmos lua check luacheck luacheck-report ast-grep ast-grep-report teal teal-report latest latest-report test home
