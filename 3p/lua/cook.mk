bins += o/%/lua/bin/lua.dist
tests += o/%/lua/test.ok
tests += o/%/lua/test_release.ok

o/%/lua/bin/lua.ape: o/%/cosmos/bin/lua $(lib_libs) $(libs) $(luaunit)
	rm -rf o/$*/lua/staging
	mkdir -p o/$*/lua/staging/.lua $(@D)
	$(foreach d,$(3p_lib_dirs),cp -r $(subst %,$*,$(d))/* o/$*/lua/staging/.lua/;)
	$(foreach d,$(lib_dirs),cp -r $(d)/* o/$*/lua/staging/.lua/;)
	cp o/$*/cosmos/bin/lua $@
	chmod +x $@
	cd o/$*/lua/staging && zip -qr $(CURDIR)/$@ .lua

o/%/lua/bin/lua.dist: o/%/lua/bin/lua.ape
	cp $< $@

o/%/lua/test.ok: 3p/lua/test.lua o/%/lua/bin/lua.dist $(runner)
	TEST_BIN_DIR=o/$*/lua $(runner) $< $@

o/%/lua/test_release.ok: 3p/lua/test_release.lua o/%/lua/bin/lua.dist $(runner)
	TEST_BIN_DIR=o/$*/lua $(runner) $< $@
