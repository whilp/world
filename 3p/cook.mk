o := $(CURDIR)/o
3p := $(o)/3p

PLATFORMS := darwin-arm64 linux-arm64 linux-x86_64

curl := curl -fsSL
sha256sum := shasum -a 256
unzip := unzip -q -DD
zip := zip -q
tar := tar -m
gunzip := gunzip -f
lua := lua

include lib/make/macros.mk
include 3p/cosmos/cook.mk
include 3p/make/cook.mk

export PATH := $(dir $(cosmos_bin)):$(PATH)

make := $(make_bin)

# fetch needs our custom lua binary with cosmo built-in
lua_bin := results/bin/lua
lib_lua = LUA_PATH="$(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;;" $(CURDIR)/$(lua_bin)
fetch := lib/build/fetch.lua

# Tools self-register via TOOLS +=
include 3p/ast-grep/cook.mk
include 3p/biome/cook.mk
include 3p/comrak/cook.mk
include 3p/delta/cook.mk
include 3p/duckdb/cook.mk
include 3p/gh/cook.mk
include 3p/marksman/cook.mk
include 3p/nvim/cook.mk
include 3p/rg/cook.mk
include 3p/ruff/cook.mk
include 3p/shfmt/cook.mk
include 3p/sqruff/cook.mk
include 3p/stylua/cook.mk
include 3p/superhtml/cook.mk
include 3p/tree-sitter/cook.mk
include 3p/uv/cook.mk

# Pattern rule template for each tool
# Generates: $(3p)/nvim/%/.extracted: 3p/nvim/version.lua ...
#   where % matches platform (darwin-arm64, linux-arm64, linux-x86_64)
define tool_download_rule
$(3p)/$(1)/%/.extracted: private .PLEDGE = stdio rpath wpath cpath inet dns exec proc
$(3p)/$(1)/%/.extracted: private .INTERNET = 1
$(3p)/$(1)/%/.extracted: private .CPU = 120
$(3p)/$(1)/%/.extracted: 3p/$(1)/version.lua $(fetch)
	@mkdir -p $$(dir $$@)
	$(lib_lua) $(fetch) 3p/$(1)/version.lua $(1) $$* $$(dir $$@)
	touch $$@
endef

# Generate pattern rule for each tool
$(foreach tool,$(TOOLS),$(eval $(call tool_download_rule,$(tool))))

# Generate {tool}_binaries variables for each tool
$(foreach tool,$(TOOLS),$(eval $(tool)_binaries := $(foreach p,$(PLATFORMS),$(3p)/$(tool)/$(p)/.extracted)))

# nvim needs plugin bundling after extraction (defined in 3p/nvim/cook.mk)
nvim_binaries := $(nvim_bundled)

# Aggregate all_binaries
all_binaries := $(foreach tool,$(TOOLS),$($(tool)_binaries))

.STRICT = 1
