uv_version := 3p/uv/version.lua
targets += o/%/uv/bin/uv
tests += o/%/uv/test.ok

o/%/uv/archive.tar.gz: $(uv_version) $(fetch)
	$(fetch) $(uv_version) $* $@

o/%/uv/staging/uv: $(uv_version) $(extract) o/%/uv/archive.tar.gz
	$(extract) $(uv_version) $* o/$*/uv/archive.tar.gz o/$*/uv/staging

o/%/uv/bin/uv: $(uv_version) $(install) o/%/uv/staging/uv
	$(install) $(uv_version) $* o/$*/uv bin o/$*/uv/staging/uv

o/%/uv/test.ok: 3p/uv/test.lua o/%/uv/bin/uv
	$< o/$*/uv && touch $@
