platforms := darwin-arm64 linux-arm64 linux-x86_64

# Detect current platform
uname_s := $(shell uname -s)
uname_m := $(shell uname -m)
ifeq ($(uname_s),Darwin)
  current_platform := darwin-$(subst x86_64,x86_64,$(subst arm64,arm64,$(uname_m)))
else ifeq ($(uname_s),Linux)
  current_platform := linux-$(subst aarch64,arm64,$(uname_m))
endif

export LUA_PATH := $(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;$(CURDIR)/o/any/luaunit/lib/?.lua;/zip/.lua/?.lua;/zip/.lua/?/init.lua
export PATH := $(CURDIR)/o/$(current_platform)/cosmos/bin:$(CURDIR)/o/any/lua/bin:$(PATH)
export RIPGREP_CONFIG_PATH := $(CURDIR)/.config/ripgrep/rg.conf

lua_bin := o/any/lua/bin/lua

# Script paths (for dependencies)
fetch_script := lib/build/fetch.lua
extract_script := lib/build/extract.lua
install_script := lib/build/install.lua
luatest_script := lib/build/luatest.lua
luacheck_script := lib/build/luacheck.lua
ast_grep_script := lib/build/ast-grep.lua
teal_script := lib/build/teal.lua

# Commands (invoke lua explicitly to avoid APE "Text file busy" errors)
fetch = $(lua_bin) $(fetch_script)
extract = $(lua_bin) $(extract_script)
install = $(lua_bin) $(install_script)
runner = $(lua_bin) $(luatest_script)
luatest_runner = $(lua_bin) $(luatest_script)
luacheck_bin = o/$(current_platform)/luacheck/bin/luacheck
luacheck_runner = $(lua_bin) $(luacheck_script)
ast_grep_runner = $(lua_bin) $(ast_grep_script)
teal_runner = $(lua_bin) $(teal_script)

luaunit := o/any/luaunit/lib/luaunit.lua

$(fetch_script) $(extract_script) $(install_script) $(luatest_script) $(luacheck_script) $(ast_grep_script) $(teal_script): | $(lua_bin)
cosmo := whilp/cosmopolitan
release ?= latest

include lib/cook.mk
include 3p/cook.mk

lua_files := $(shell rg --files -g '*.lua'; rg --no-ignore -l '^#!/.*lua' -g '!*.lua' -g '!o/' 2>/dev/null)
test_files := $(shell rg --files -g '*test.lua' -g 'test_*.lua' | grep -vE '(latest|luatest)\.lua$$')
luatest_files := $(patsubst %,o/any/%.luatest.ok,$(test_files))
luacheck_files := $(patsubst %,o/any/%.luacheck.ok,$(lua_files))

luatest: $(luatest_files) ## Run tests incrementally on changed files

o/any/%.luatest.ok: % $(luatest_script) $(luaunit) o/any/walk/lib/walk/init.lua
	$(TEST_ENV) $(luatest_runner) $< $@ $(TEST_ARGS)

luatest-report: $(luatest_files) o/any/walk/lib/walk/init.lua ## Run tests and show summary report
	@$(luatest_runner) report o/any

luacheck: $(luacheck_files) ## Run luacheck incrementally on changed files

o/any/%.luacheck.ok: % .luacheckrc $(luacheck_script) $(luacheck_bin)
	$(luacheck_runner) $< $@ $(luacheck_bin)

luacheck-report: $(luacheck_files) ## Run luacheck and show summary report
	@$(luacheck_runner) report o/any

ast_grep_files := $(patsubst %,o/any/%.ast-grep.ok,$(lua_files))

ast-grep: $(ast_grep_files) ## Run ast-grep incrementally on changed files

o/any/%.ast-grep.ok: % sgconfig.yml $(ast_grep_script) $(ast_grep)
	$(ast_grep_runner) $< $@ $(ast_grep)

ast-grep-report: $(ast_grep_files) ## Run ast-grep and show summary report
	@$(ast_grep_runner) report o/any

teal_files := $(patsubst %,o/any/%.teal.ok,$(lua_files))

teal: $(teal_files) ## Run teal incrementally on changed files

o/any/%.teal.ok: % $(teal_script) $(tl_bin) $(lua_dist)
	$(teal_runner) $< $@ $(tl_bin) $(lua_dist)

teal-report: $(teal_files) ## Run teal and show summary report
	# TODO: remove || true once all files pass
	@$(teal_runner) report o/any || true

bootstrap: $(lua_bin)
	@[ -n "$$CLAUDE_ENV_FILE" ] && echo "PATH=$(dir $(lua_bin)):\$$PATH" >> "$$CLAUDE_ENV_FILE"; true

$(lua_bin):
	@mkdir -p $(@D)
	curl -sL -o $@ "https://github.com/$(cosmo)/releases/$(release)/download/lua"
	@chmod +x $@

cosmos: o/$(current_platform)/cosmos/bin/lua
lua: o/$(current_platform)/lua/bin/lua.dist

ast_grep := o/$(current_platform)/ast-grep/bin/ast-grep
lua_dist := o/$(current_platform)/lua/bin/lua.dist

tl_bin := o/$(current_platform)/tl/bin/tl

check: $(ast_grep_files) $(luacheck_files) $(teal_files) ## Run ast-grep, luacheck, and teal
	@$(ast_grep_runner) report o/any
	@echo ""
	@$(luacheck_runner) report o/any
	@echo ""
	# TODO: remove || true once all files pass teal
	@$(teal_runner) report o/any || true

test: $(filter o/any/lib/% o/any/3p/nvim/%,$(luatest_files)) $(subst %,$(current_platform),$(tests))
	@echo "All tests passed"

clean:
	rm -rf o

.PHONY: bootstrap clean cosmos lua check luacheck luacheck-report ast-grep ast-grep-report teal teal-report test home
