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

lua_bin := o/any/lua/bin/lua

# Script paths (for dependencies)
fetch_script := lib/build/fetch.lua
extract_script := lib/build/extract.lua
install_script := lib/build/install.lua
runner_script := lib/build/test.lua

# Commands (invoke lua explicitly to avoid APE "Text file busy" errors)
fetch = $(lua_bin) $(fetch_script)
extract = $(lua_bin) $(extract_script)
install = $(lua_bin) $(install_script)
runner = $(lua_bin) $(runner_script)

luaunit := o/any/luaunit/lib/luaunit.lua

$(fetch_script) $(extract_script) $(install_script) $(runner_script): | $(lua_bin)
cosmo := whilp/cosmopolitan
release ?= latest

include lib/cook.mk
include 3p/cook.mk

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

check: $(ast_grep) $(lua_dist) $(tl_bin) ## Run ast-grep, luacheck, and teal
	@echo "Running ast-grep..."
	$(ast_grep) scan --color always
	@echo ""
	@echo "Running luacheck..."
	$(lua_dist) -e 'arg={[0]="luacheck","."} require("luacheck.main")'
	@echo ""
	@echo "Running teal type checker (lax mode)..."
	-@$(lua_dist) $(tl_bin) check lib/*.lua lib/**/*.lua 3p/*/*.lua 2>&1

test: lib-test $(subst %,$(current_platform),$(tests))
	@echo "All tests passed"

clean:
	rm -rf o

.PHONY: bootstrap clean cosmos lua check test home
