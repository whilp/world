cosmos_version := 3p/cosmos/version.lua
cosmos_bins := o/%/cosmos/bin/lua o/%/cosmos/bin/zip o/%/cosmos/bin/unzip o/%/cosmos/bin/make
bins += $(cosmos_bins)
tests += o/%/cosmos/test.ok

o/%/cosmos/archive.zip: $(cosmos_version) $(fetch)
	$(fetch) $(cosmos_version) $* $@

o/%/cosmos/staging/lua: $(cosmos_version) $(extract) o/%/cosmos/archive.zip
	$(extract) $(cosmos_version) $* o/$*/cosmos/archive.zip o/$*/cosmos/staging

$(cosmos_bins): $(cosmos_version) $(install) o/%/cosmos/staging/lua
	$(install) $(cosmos_version) $* o/$*/cosmos bin o/$*/cosmos/staging/$(@F)

o/%/cosmos/test.ok: 3p/cosmos/test.lua $(cosmos_bins) $(runner)
	TEST_BIN_DIR=o/$*/cosmos $(runner) $< $@

cosmos-latest: | $(lua_bin)
	lua 3p/cosmos/latest.lua > $(cosmos_version)

.PHONY: cosmos-latest
