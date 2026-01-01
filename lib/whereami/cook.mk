lib_lua_modules += whereami
lib_dirs += o/any/whereami/lib
lib_libs += o/any/whereami/lib/whereami/init.lua
lib_tests += o/any/whereami/test.ok

o/any/whereami/lib/whereami/init.lua: lib/whereami/init.lua
	mkdir -p $(@D)
	cp $< $@

o/any/whereami/test.ok: lib/whereami/test.lua o/any/whereami/lib/whereami/init.lua $(runner)
	$(runner) $< $@
