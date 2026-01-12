modules += claude
claude_srcs := $(wildcard lib/claude/*.tl)
claude_tests := lib/claude/test.tl
lib_lua_modules += claude
lib_dirs += o/any/claude/lib
lib_libs += o/any/claude/lib/claude/main.lua

# uses secondary expansion so $(tl_files) is evaluated after 3p/tl/cook.mk
.SECONDEXPANSION:
o/any/claude/lib/claude/main.lua: lib/claude/main.tl $(types_files) $$(tl_files) | $$(tl_staged)
	@mkdir -p $(@D)
	@$(tl_gen) -o $@ $<

# test depends on module being compiled
$(o)/lib/claude/test.tl.test.ok: o/any/claude/lib/claude/main.lua
