modules += aerosnap
aerosnap_srcs := $(wildcard lib/aerosnap/*.tl)
aerosnap_tests := lib/aerosnap/test.tl
aerosnap_files := o/any/lib/aerosnap/init.lua
lib_lua_modules += aerosnap
lib_libs += o/any/lib/aerosnap/init.lua

# test depends on module being compiled
$(o)/lib/aerosnap/test.tl.test.ok: o/any/lib/aerosnap/init.lua
