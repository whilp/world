lib_lua_modules += environ
lib_dirs += o/any/environ/lib
lib_libs += o/any/environ/lib/environ/init.lua

o/any/environ/lib/environ/init.lua: lib/environ/init.lua
	mkdir -p $(@D)
	cp $< $@
