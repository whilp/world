modules += environ
environ_srcs := $(wildcard lib/environ/*.tl)
environ_tests := lib/environ/test.tl
lib_lua_modules += environ
lib_libs += o/any/lib/environ/init.lua

# test depends on module being compiled
$(o)/lib/environ/test.tl.test.ok: o/any/lib/environ/init.lua
