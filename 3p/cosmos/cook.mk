cosmos_version := 3p/cosmos/version.lua
bins += o/%/cosmos/bin/lua
tests += o/%/cosmos/test.ok

o/%/cosmos/archive.zip: $(cosmos_version) $(fetch)
	$(fetch) $(cosmos_version) $* $@

o/%/cosmos/staging/lua: $(cosmos_version) $(extract) o/%/cosmos/archive.zip
	$(extract) $(cosmos_version) $* o/$*/cosmos/archive.zip o/$*/cosmos/staging

o/%/cosmos/bin/lua: $(cosmos_version) $(install) o/%/cosmos/staging/lua
	$(install) $(cosmos_version) $* o/$*/cosmos bin o/$*/cosmos/staging/lua

o/%/cosmos/test.ok: 3p/cosmos/test.lua o/%/cosmos/bin/lua $(runner)
	TEST_BIN_DIR=o/$*/cosmos $(runner) $< $@

cosmos-latest: | $(lua_bin)
	lua 3p/cosmos/latest.lua > $(cosmos_version)

.PHONY: cosmos-latest
