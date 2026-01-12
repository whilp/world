lua_dist := $(o_platform)/lua/bin/lua.dist
bins += o/%/lua/bin/lua.dist

$(luatest_o)/3p/lua/test.lua.ok: $(lua_dist)
$(luatest_o)/3p/lua/test.lua.ok: TEST_ENV = TEST_BIN_DIR=$(o_platform)/lua

$(luatest_o)/3p/lua/test_release.lua.ok: $(lua_dist)
$(luatest_o)/3p/lua/test_release.lua.ok: TEST_ENV = TEST_BIN_DIR=$(o_platform)/lua

o/%/lua/bin/lua.ape: o/%/cosmos/bin/lua $(lib_libs) $(libs)
	@rm -rf o/$*/lua/staging
	@mkdir -p o/$*/lua/staging/.lua $(@D)
	@$(foreach d,$(3p_lib_dirs),cp -r $(subst %,$*,$(d))/* o/$*/lua/staging/.lua/;)
	@$(foreach d,$(lib_dirs),cp -r $(d)/* o/$*/lua/staging/.lua/;)
	@# Remove test files and build artifacts from staging
	@find o/$*/lua/staging/.lua -name 'test*.lua' -delete
	@find o/$*/lua/staging/.lua -name 'cook.mk' -delete
	@find o/$*/lua/staging/.lua -name '*.ok' -delete
	@cp o/$*/cosmos/bin/lua $@
	@chmod +x $@
	@cd o/$*/lua/staging && zip -qr $(CURDIR)/$@ .lua

o/%/lua/bin/lua.dist: o/%/lua/bin/lua.ape
	@cp $< $@

lua-all: $(foreach p,$(platforms),o/$(p)/lua/bin/lua.dist) ## Build lua.dist for all platforms

.PHONY: lua-all
