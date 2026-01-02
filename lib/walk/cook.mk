# lib/walk - directory tree walking utilities

lib_dirs += o/any/walk/lib
lib_libs += o/any/walk/lib/walk/init.lua
lib_tests += o/any/walk/test_walk.ok

o/any/walk/lib/walk/init.lua: lib/walk/init.lua
	mkdir -p $(@D)
	cp $< $@

o/any/walk/test_walk.ok: lib/walk/test_walk.lua o/any/walk/lib/walk/init.lua $(runner)
	$(runner) $< $@
