biome_version := 3p/biome/version.lua
bins += o/%/biome/bin/biome

o/luatest/3p/biome/test.lua.ok: o/$(current_platform)/biome/bin/biome
o/luatest/3p/biome/test.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/biome

o/%/biome/download: $(biome_version) $(fetch)
	$(fetch) $(biome_version) $* $@

o/%/biome/bin/biome: $(biome_version) $(install) o/%/biome/download
	$(install) $(biome_version) $* o/$*/biome bin o/$*/biome/download
