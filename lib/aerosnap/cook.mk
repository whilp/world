lib_lua_modules += aerosnap
lib_dirs += o/any/aerosnap/lib
lib_libs += o/any/aerosnap/lib/aerosnap/init.lua
lib_tests += o/any/aerosnap/test.ok

o/any/aerosnap/lib/aerosnap/init.lua: lib/aerosnap/init.lua
	mkdir -p $(@D)
	cp $< $@

o/any/aerosnap/test.ok: lib/aerosnap/test.lua o/any/aerosnap/lib/aerosnap/init.lua $(runner)
	$(runner) $< $@
