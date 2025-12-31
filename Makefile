platforms := darwin-arm64 linux-arm64 linux-x86_64

# Detect current platform
uname_s := $(shell uname -s)
uname_m := $(shell uname -m)
ifeq ($(uname_s),Darwin)
  current_platform := darwin-$(subst x86_64,x86_64,$(subst arm64,arm64,$(uname_m)))
else ifeq ($(uname_s),Linux)
  current_platform := linux-$(subst aarch64,arm64,$(uname_m))
endif

export LUA_PATH := $(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;;
export PATH := $(CURDIR)/o/$(current_platform)/cosmos/bin:$(CURDIR)/o/any/lua/bin:$(PATH)

lua_bin := o/any/lua/bin/lua
fetch := lib/build/fetch.lua
extract := lib/build/extract.lua
install := lib/build/install.lua

$(fetch) $(extract) $(install): | $(lua_bin)
cosmo := whilp/cosmopolitan
release ?= latest

include 3p/cook.mk

bootstrap: $(lua_bin)
	@[ -n "$$CLAUDE_ENV_FILE" ] && echo "PATH=$(dir $(lua_bin)):\$$PATH" >> "$$CLAUDE_ENV_FILE"; true

$(lua_bin):
	@mkdir -p $(@D)
	curl -sL -o $@ "https://github.com/$(cosmo)/releases/$(release)/download/lua"
	@chmod +x $@

cosmos: o/$(current_platform)/cosmos/bin/lua
lua: cosmos

# TODO: rewrite these targets
home:
	@echo "TODO: home target not yet rewritten"

check:
	@echo "TODO: check target not yet rewritten"

test:
	@echo "TODO: test target not yet rewritten"

clean:
	rm -rf o

.PHONY: bootstrap clean cosmos lua home check test
