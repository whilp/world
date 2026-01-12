modules += claude
claude_srcs := $(wildcard lib/claude/*.tl)
claude_tests := lib/claude/test.tl
lib_lua_modules += claude
lib_libs += o/any/lib/claude/main.lua

# test depends on module being compiled
$(o)/lib/claude/test.tl.test.ok: o/any/lib/claude/main.lua
