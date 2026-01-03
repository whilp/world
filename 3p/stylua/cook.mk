stylua_version := 3p/stylua/version.lua
bins += o/%/stylua/bin/stylua

o/%/stylua/archive.zip: $(stylua_version) $(fetch)
	$(fetch) $(stylua_version) $* $@

o/%/stylua/staging/stylua: $(stylua_version) $(extract) o/%/stylua/archive.zip
	$(extract) $(stylua_version) $* o/$*/stylua/archive.zip o/$*/stylua/staging

o/%/stylua/bin/stylua: $(stylua_version) $(install) o/%/stylua/staging/stylua
	$(install) $(stylua_version) $* o/$*/stylua bin o/$*/stylua/staging/stylua
