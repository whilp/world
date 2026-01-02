# lib/walk - directory tree walking utilities

lib_dirs += o/any/walk/lib
lib_libs += o/any/walk/lib/walk/init.lua

o/any/walk/lib/walk/init.lua: lib/walk/init.lua
	mkdir -p $(@D)
	cp $< $@
