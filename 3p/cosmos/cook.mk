cosmos_version := 3p/cosmos/version.lua
cosmos_bins := o/%/cosmos/bin/lua o/%/cosmos/bin/zip o/%/cosmos/bin/unzip o/%/cosmos/bin/make
bins += $(cosmos_bins)
tests += o/%/cosmos/test.ok

o/%/cosmos/archive.zip: $(cosmos_version) $(fetch)
	$(fetch) $(cosmos_version) $* $@

o/%/cosmos/.extracted: $(cosmos_version) $(extract) o/%/cosmos/archive.zip
	$(extract) $(cosmos_version) $* o/$*/cosmos/archive.zip o/$*/cosmos/staging
	touch $@

# each cosmos bin needs its own explicit rule
o/%/cosmos/bin/lua: $(cosmos_version) $(install) o/%/cosmos/.extracted
	$(install) $(cosmos_version) $* o/$*/cosmos bin o/$*/cosmos/staging/lua

o/%/cosmos/bin/zip: $(cosmos_version) $(install) o/%/cosmos/.extracted
	$(install) $(cosmos_version) $* o/$*/cosmos bin o/$*/cosmos/staging/zip

o/%/cosmos/bin/unzip: $(cosmos_version) $(install) o/%/cosmos/.extracted
	$(install) $(cosmos_version) $* o/$*/cosmos bin o/$*/cosmos/staging/unzip

o/%/cosmos/bin/make: $(cosmos_version) $(install) o/%/cosmos/.extracted
	$(install) $(cosmos_version) $* o/$*/cosmos bin o/$*/cosmos/staging/make

o/%/cosmos/test.ok: 3p/cosmos/test.lua $(cosmos_bins) $(runner)
	TEST_BIN_DIR=o/$*/cosmos $(runner) $< $@
