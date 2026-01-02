# lib/build - build tools (fetch, extract, install)

lib_dirs += o/any/build/lib
lib_libs += o/any/build/lib/build/install.lua
lib_libs += o/any/build/lib/build/fetch.lua
lib_libs += o/any/build/lib/build/review.lua
lib_tests += o/any/build/test_install.ok
lib_tests += o/any/build/test_fetch.ok
lib_tests += o/any/build/test_extract.ok
lib_tests += o/any/build/test_luacheck.ok
lib_tests += o/any/build/test_review.ok

o/any/build/lib/build/%.lua: lib/build/%.lua
	mkdir -p $(@D)
	cp $< $@

o/any/build/test_install.ok: lib/build/test_install.lua o/any/build/lib/build/install.lua $(runner)
	$(runner) $< $@

o/any/build/test_fetch.ok: lib/build/test_fetch.lua o/any/build/lib/build/fetch.lua $(runner)
	$(runner) $< $@

o/any/build/test_extract.ok: lib/build/test_extract.lua lib/build/extract.lua $(runner)
	$(runner) $< $@

o/any/build/test_luacheck.ok: lib/build/test_luacheck.lua lib/build/luacheck.lua $(luacheck_bin) $(runner)
	TEST_BIN_DIR=o/$(current_platform)/luacheck $(runner) $< $@ $(CURDIR)/.luacheckrc

o/any/build/test_review.ok: lib/build/test_review.lua o/any/build/lib/build/review.lua $(runner)
	$(runner) $< $@
