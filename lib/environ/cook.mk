lib_lua_modules += environ
lib_dirs += o/any/environ/lib
lib_libs += o/any/environ/lib/environ/init.lua
lib_tests += o/any/environ/test.ok

o/any/environ/lib/environ/init.lua: lib/environ/init.lua
	mkdir -p $(@D)
	cp $< $@

o/any/environ/test.ok: lib/environ/test.lua o/any/environ/lib/environ/init.lua $(runner)
	$(runner) $< $@
