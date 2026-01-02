bins += o/%/lua/bin/lua.dist

o/any/3p/lua/test.lua.luatest.ok: o/$(current_platform)/lua/bin/lua.dist
o/any/3p/lua/test.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/lua

o/any/3p/lua/test_release.lua.luatest.ok: o/$(current_platform)/lua/bin/lua.dist
o/any/3p/lua/test_release.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/lua

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

lua-all: $(foreach p,$(platforms),o/$(p)/lua/bin/lua.dist) ## Build lua.dist for all platforms

.PHONY: lua-all
