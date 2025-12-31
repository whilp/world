biome_version := 3p/biome/version.lua
bins += o/%/biome/bin/biome
tests += o/%/biome/test.ok

o/%/biome/download: $(biome_version) $(fetch)
	$(fetch) $(biome_version) $* $@

o/%/biome/bin/biome: $(biome_version) $(install) o/%/biome/download
	$(install) $(biome_version) $* o/$*/biome bin o/$*/biome/download

o/%/biome/test.ok: 3p/biome/test.lua o/%/biome/bin/biome
	$< o/$*/biome && touch $@
