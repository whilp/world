nvim_version := 3p/nvim/version.lua
targets += o/%/nvim/bin/nvim
tests += o/%/nvim/test.ok

o/%/nvim/archive.tar.gz: $(nvim_version) $(fetch)
	$(fetch) $(nvim_version) $* $@

o/%/nvim/staging/bin/nvim: $(nvim_version) $(extract) o/%/nvim/archive.tar.gz
	$(extract) $(nvim_version) $* o/$*/nvim/archive.tar.gz o/$*/nvim/staging

o/%/nvim/bin/nvim: $(nvim_version) $(install) o/%/nvim/staging/bin/nvim
	$(install) $(nvim_version) $* o/$*/nvim/staging/bin/nvim o/$*/nvim

o/%/nvim/test.ok: 3p/nvim/test.lua o/%/nvim/bin/nvim
	$(runner) $< o/$*/nvim $@

nvim-latest: | $(lua_bin)
	3p/nvim/latest.lua > 3p/nvim/version.lua

.PHONY: nvim-latest
