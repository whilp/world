lib_lua_modules += aerosnap
lib_dirs += o/any/aerosnap/lib
lib_libs += o/any/aerosnap/lib/aerosnap/init.lua

o/any/aerosnap/lib/aerosnap/init.lua: lib/aerosnap/init.tl $(types_files) | $(tl_staged)
	mkdir -p $(@D)
	$(tl_gen) -o $@ $<
