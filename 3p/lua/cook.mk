bins += o/%/lua/bin/lua.dist
tests += o/%/lua/test.ok

o/%/lua/bin/lua.ape: o/%/cosmos/bin/lua $(lib_libs) $(libs)
	rm -rf o/$*/lua/staging
	mkdir -p o/$*/lua/staging/.lua o/$*/lua/staging/.lua/bin $(@D)
	$(foreach d,$(3p_lib_dirs),cp -r $(subst %,$*,$(d))/* o/$*/lua/staging/.lua/;)
	$(foreach d,$(lib_dirs),cp -r $(d)/* o/$*/lua/staging/.lua/;)
	cp 3p/luacheck/luacheck o/$*/lua/staging/.lua/bin/luacheck
	find o/$*/lua/staging/.lua -name '*.ok' -delete
	find o/$*/lua/staging/.lua -name 'test*.lua' -delete
	find o/$*/lua/staging/.lua -name 'cook.mk' -delete
	cp o/$*/cosmos/bin/lua $@
	chmod +x $@
	cd o/$*/lua/staging && zip -qr $(CURDIR)/$@ .lua

o/%/lua/bin/lua.dist: o/%/lua/bin/lua.ape
	cp $< $@
	./$@ --assimilate || true

o/%/lua/test.ok: 3p/lua/test.lua o/%/lua/bin/lua.dist $(runner)
	$(runner) $< $@
