o := $(CURDIR)/o

PLATFORMS := darwin-arm64 linux-arm64 linux-x86_64

curl := curl -fsSL
sha256sum := shasum -a 256
unzip := unzip -q -DD
zip := zip -q
tar := tar -m
gunzip := gunzip -f

include lib/make/macros.mk
include 3p/cosmos/cook.mk
include 3p/make/cook.mk

export PATH := $(dir $(cosmos_bin)):$(PATH)

make := $(make_bin)

# fetch needs our custom lua binary with cosmo built-in
lua_bin := o/any/bin/lua
fetch := lib/build/fetch.lua

# each tool defines its own download rule and {tool}_binaries variable
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

# nvim uses .bundled instead of .extracted
nvim_binaries := $(nvim_bundled)

all_binaries := \
	$(ast-grep_binaries) \
	$(biome_binaries) \
	$(comrak_binaries) \
	$(delta_binaries) \
	$(duckdb_binaries) \
	$(gh_binaries) \
	$(marksman_binaries) \
	$(nvim_binaries) \
	$(rg_binaries) \
	$(ruff_binaries) \
	$(shfmt_binaries) \
	$(sqruff_binaries) \
	$(stylua_binaries) \
	$(superhtml_binaries) \
	$(tree-sitter_binaries) \
	$(uv_binaries)

.STRICT = 1
