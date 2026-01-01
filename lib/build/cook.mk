# lib/build - build tools (fetch, extract, install)

lib_dirs += o/any/build/lib
lib_libs += o/any/build/lib/build/install.lua
lib_tests += o/any/build/test_install.ok

o/any/build/lib/build/install.lua: lib/build/install.lua
	mkdir -p $(@D)
	cp $< $@

o/any/build/test_install.ok: lib/build/test_install.lua o/any/build/lib/build/install.lua $(runner)
	$(runner) $< $@
