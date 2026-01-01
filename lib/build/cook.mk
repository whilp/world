# lib/build - build tools (fetch, extract, install)

lib_dirs += o/any/build/lib
lib_libs += o/any/build/lib/build/install.lua
lib_libs += o/any/build/lib/build/fetch.lua
lib_tests += o/any/build/test_install.ok
lib_tests += o/any/build/test_fetch.ok
lib_tests += o/any/build/test_extract.ok

o/any/build/lib/build/install.lua: lib/build/install.lua
	mkdir -p $(@D)
	cp $< $@

o/any/build/lib/build/fetch.lua: lib/build/fetch.lua
	mkdir -p $(@D)
	cp $< $@

o/any/build/test_install.ok: lib/build/test_install.lua o/any/build/lib/build/install.lua $(runner)
	$(runner) $< $@

o/any/build/test_fetch.ok: lib/build/test_fetch.lua o/any/build/lib/build/fetch.lua $(runner)
	$(runner) $< $@

o/any/build/test_extract.ok: lib/build/test_extract.lua lib/build/extract.lua $(runner)
	$(runner) $< $@
