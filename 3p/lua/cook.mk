bins += o/%/lua/bin/lua.dist

o/%/lua/bin/lua.ape: o/%/cosmos/bin/lua $(lib_libs) $(libs)
	rm -rf o/$*/lua/staging
	mkdir -p o/$*/lua/staging/.lua $(@D)
	$(foreach d,$(3p_lib_dirs),cp -r $(subst %,$*,$(d))/* o/$*/lua/staging/.lua/;)
	$(foreach d,$(lib_dirs),cp -r $(d)/* o/$*/lua/staging/.lua/;)
	cp o/$*/cosmos/bin/lua $@
	chmod +x $@
	cd o/$*/lua/staging && zip -qr $(CURDIR)/$@ .lua

o/%/lua/bin/lua.dist: o/%/lua/bin/lua.ape
	cp $< $@
	./$@ --assimilate || true
