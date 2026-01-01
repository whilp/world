bins += o/%/lua-dist/bin/lua

o/%/lua-dist/bin/lua.ape: o/%/cosmos/bin/lua $(lib_libs) $(libs)
	rm -rf o/$*/lua-dist/staging
	mkdir -p o/$*/lua-dist/staging/.lua $(@D)
	$(foreach d,$(3p_lib_dirs),cp -r $(subst %,$*,$(d))/* o/$*/lua-dist/staging/.lua/;)
	$(foreach d,$(lib_dirs),cp -r $(d)/* o/$*/lua-dist/staging/.lua/;)
	cp o/$*/cosmos/bin/lua $@
	chmod +x $@
	cd o/$*/lua-dist/staging && zip -qr $(CURDIR)/$@ .lua

o/%/lua-dist/bin/lua: o/%/lua-dist/bin/lua.ape
	cp $< $@
	./$@ --assimilate || true
