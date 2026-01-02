# lib/walk - directory tree walking utilities

lib_tests += o/any/walk/test_walk.ok

o/any/walk/test_walk.ok: lib/walk/test_walk.lua lib/walk/init.lua $(runner)
	$(runner) $< $@
