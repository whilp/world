# platform detection
platforms := darwin-arm64 linux-arm64 linux-x86_64

uname_s := $(shell uname -s)
uname_m := $(shell uname -m)
ifeq ($(uname_s),Darwin)
  current_platform := darwin-$(subst x86_64,x86_64,$(subst arm64,arm64,$(uname_m)))
else ifeq ($(uname_s),Linux)
  current_platform := linux-$(subst aarch64,arm64,$(uname_m))
endif

# output directories
o := o
o_platform := $(o)/$(current_platform)
o_any := $(o)/any

# bootstrap configuration
cosmo := whilp/cosmopolitan
release ?= latest
cosmic_release := home-2026-01-02-75caba6

lua_bin := $(o_any)/lua/bin/lua

export PATH := $(CURDIR)/$(o_platform)/cosmos/bin:$(CURDIR)/$(o_any)/lua/bin:$(PATH)

# manifest infrastructure (defined early so cook.mk files can reference them)
manifest_o := $(o)/manifest
manifest_git := $(manifest_o)/git.txt
manifest_luafiles := $(manifest_o)/lua-files.txt
manifest_luatests := $(manifest_o)/lua-tests.txt

# include project modules
include lib/cook.mk
include 3p/cook.mk
include build.mk

# lua path construction
null :=
space := $(null) $(null)
lib_paths := $(subst $(space),,$(foreach dir,$(lib_dirs),$(CURDIR)/$(dir)/?.lua;$(CURDIR)/$(dir)/?/init.lua;))
3p_lib_paths := $(subst $(space),,$(foreach dir,$(subst %,$(current_platform),$(3p_lib_dirs)),$(CURDIR)/$(dir)/?.lua;$(CURDIR)/$(dir)/?/init.lua;))
export LUA_PATH := $(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;$(lib_paths)$(3p_lib_paths)/zip/.lua/?.lua;/zip/.lua/?/init.lua

# bootstrap lua interpreter
$(lua_bin):
	@mkdir -p $(@D)
	curl -sL -o $@ "https://github.com/whilp/world/releases/download/$(cosmic_release)/cosmic-lua"
	@chmod +x $@

bootstrap: $(lua_bin)
	@[ -n "$$CLAUDE_ENV_FILE" ] && echo "PATH=$(dir $(lua_bin)):\$$PATH" >> "$$CLAUDE_ENV_FILE"; true

# convenience targets
cosmos: o/$(current_platform)/cosmos/bin/lua
lua: o/$(current_platform)/lua/bin/lua.dist

# aggregate targets
check: $(ast_grep_files) $(luacheck_files) $(teal_files) ## Run ast-grep, luacheck, and teal
	@$(ast_grep_runner) report o/ast-grep
	@echo ""
	@$(luacheck_runner) report o/luacheck
	@echo ""
	# TODO: remove || true once all files pass teal
	@$(teal_runner) report o/teal || true

test: $(luatest_files) ## Run all tests
	@echo "All tests passed"

clean: ## Remove all build outputs
	rm -rf o

# lib/build test dependencies (after includes so variables are available)
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

.PHONY: bootstrap clean cosmos lua check luacheck luacheck-report ast-grep ast-grep-report teal teal-report latest latest-report test home
