stylua_version := 3p/stylua/version.lua
targets += o/%/stylua/bin/stylua
tests += o/%/stylua/test.ok

o/%/stylua/archive.zip: $(stylua_version) $(fetch)
	$(fetch) $(stylua_version) $* $@

o/%/stylua/staging/stylua: $(stylua_version) $(extract) o/%/stylua/archive.zip
	$(extract) $(stylua_version) $* o/$*/stylua/archive.zip o/$*/stylua/staging

o/%/stylua/bin/stylua: $(stylua_version) $(install) o/%/stylua/staging/stylua
	$(install) $(stylua_version) $* o/$*/stylua/staging/stylua o/$*/stylua

o/%/stylua/test.ok: o/%/stylua/bin/sty3p/stylua/test.lua
	$< o/$*/stylua && touch $@
