lib_lua_modules += whereami
lib_dirs += o/any/whereami/lib
lib_libs += o/any/whereami/lib/whereami/init.lua

o/any/whereami/lib/whereami/init.lua: lib/whereami/init.tl $(types_files) | $(tl_staged)
	mkdir -p $(@D)
	$(tl_gen) -o $@ $<
