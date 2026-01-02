lib_lua_modules += daemonize
lib_dirs += o/any/daemonize/lib
lib_libs += o/any/daemonize/lib/daemonize/init.lua

o/any/daemonize/lib/daemonize/init.lua: lib/daemonize/init.lua
	mkdir -p $(@D)
	cp $< $@
