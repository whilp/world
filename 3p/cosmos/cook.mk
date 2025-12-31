cosmos_version := 3p/cosmos/version.lua
targets += o/any/cosmos/bin/lua
tests += o/any/cosmos/test.ok

o/any/cosmos/archive.zip: $(cosmos_version) $(fetch)
	$(fetch) $(cosmos_version) any $@

o/any/cosmos/staging/lua: $(cosmos_version) $(extract) o/any/cosmos/archive.zip
	$(extract) $(cosmos_version) any o/any/cosmos/archive.zip o/any/cosmos/staging

o/any/cosmos/bin/lua: $(cosmos_version) $(install) o/any/cosmos/staging/lua
	$(install) $(cosmos_version) any o/any/cosmos/staging o/any/cosmos

o/any/cosmos/test.ok: 3p/cosmos/test.lua o/any/cosmos/bin/lua
	$< o/any/cosmos && touch $@

cosmos-latest: | $(lua_bin)
	lua 3p/cosmos/latest.lua > $(cosmos_version)

.PHONY: cosmos-latest
