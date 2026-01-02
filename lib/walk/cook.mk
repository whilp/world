# lib/walk - directory tree walking utilities

lib_dirs += o/any/walk/lib
walk_lib := o/any/walk/lib/walk/init.lua
lib_libs += $(walk_lib)

$(walk_lib): lib/walk/init.lua
	mkdir -p $(@D)
	cp $< $@
