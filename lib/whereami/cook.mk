lib_lua_modules += whereami
lib_dirs += o/any/whereami/lib
lib_libs += o/any/whereami/lib/whereami/init.lua

o/any/whereami/lib/whereami/init.lua: lib/whereami/init.lua
	mkdir -p $(@D)
	cp $< $@
