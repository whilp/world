3p_lib_dirs :=

include 3p/argparse/cook.mk
include 3p/ast-grep/cook.mk
include 3p/biome/cook.mk
include 3p/comrak/cook.mk
include 3p/cosmos/cook.mk
include 3p/delta/cook.mk
include 3p/duckdb/cook.mk
include 3p/gh/cook.mk
include 3p/lfs/cook.mk
include 3p/luacheck/cook.mk
include 3p/luaunit/cook.mk
include 3p/marksman/cook.mk
include 3p/nvim/cook.mk
include 3p/rg/cook.mk
include 3p/ruff/cook.mk
include 3p/shfmt/cook.mk
include 3p/sqruff/cook.mk
include 3p/stylua/cook.mk
include 3p/superhtml/cook.mk
include 3p/tl/cook.mk
include 3p/tree-sitter/cook.mk
include 3p/uv/cook.mk
include 3p/lua/cook.mk

define platform_target
3p-$(1): $(subst %,$(1),$(bins) $(libs))
endef
$(foreach p,$(platforms),$(eval $(call platform_target,$(p))))

# Add 3p libs to global LUA_PATH
null :=
space := $(null) $(null)
LUA_PATH_3P := $(subst $(space),,$(foreach lib,$(lua_libs),o/$(current_platform)/$(lib)/lib/?.lua;o/$(current_platform)/$(lib)/lib/?/init.lua;))
export LUA_PATH := $(LUA_PATH);$(LUA_PATH_3P)
