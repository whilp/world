platforms := darwin-arm64 linux-arm64 linux-x86_64

export LUA_PATH := $(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;;
export PATH := $(CURDIR)/o/any/lua/bin:$(PATH)

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

clean:
	rm -rf o

.PHONY: bootstrap clean
