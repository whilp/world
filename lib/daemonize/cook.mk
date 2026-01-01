lib_lua_modules += daemonize
lib_dirs += o/any/daemonize/lib
lib_libs += o/any/daemonize/lib/daemonize/init.lua
lib_tests += o/any/daemonize/test.ok

o/any/daemonize/lib/daemonize/init.lua: lib/daemonize/init.lua
	mkdir -p $(@D)
	cp $< $@

o/any/daemonize/test.ok: lib/daemonize/test.lua o/any/daemonize/lib/daemonize/init.lua o/any/spawn/lib/spawn/init.lua $(runner)
	$(runner) $< $@
