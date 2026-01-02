nvim_version := 3p/nvim/version.lua
nvim_pack_lock := .config/nvim/nvim-pack-lock.json
nvim_plugins := conform.nvim mini.nvim nvim-lspconfig nvim-treesitter
nvim_fetch_plugin := 3p/nvim/fetch-plugin.lua
nvim_bundle := 3p/nvim/bundle.lua
bins += o/%/nvim/bin/nvim

# Test uses generic luatest pattern with target-specific prereqs
o/any/3p/nvim/test.lua.luatest.ok: o/$(current_platform)/nvim/bin/nvim
o/any/3p/nvim/test.lua.luatest.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/nvim

o/%/nvim/archive.tar.gz: $(nvim_version) $(fetch)
	$(fetch) $(nvim_version) $* $@

o/%/nvim/staging/bin/nvim: $(nvim_version) $(extract) o/%/nvim/archive.tar.gz
	$(extract) $(nvim_version) $* o/$*/nvim/archive.tar.gz o/$*/nvim/staging

# Fetch each plugin (keep cached across builds)
o/any/nvim/plugins/%: $(nvim_pack_lock) $(nvim_fetch_plugin)
	$(nvim_fetch_plugin) $* $@

nvim_plugin_dirs := $(addprefix o/any/nvim/plugins/,$(nvim_plugins))
.PRECIOUS: $(nvim_plugin_dirs)

# Serialize plugin fetching to avoid race conditions with parallel builds
o/any/nvim/.plugins-fetched: $(nvim_plugin_dirs)
	mkdir -p $(@D)
	touch $@

o/%/nvim/bin/nvim: $(nvim_version) $(install) o/%/nvim/staging/bin/nvim o/any/nvim/.plugins-fetched $(nvim_bundle)
	$(install) $(nvim_version) $* o/$*/nvim bin o/$*/nvim/staging/bin/nvim
	$(install) $(nvim_version) $* o/$*/nvim share o/$*/nvim/staging/share
	$(nvim_bundle) $* o/$*/nvim o/any/nvim/plugins

nvim-latest: | $(lua_bin)
	3p/nvim/latest.lua > 3p/nvim/version.lua

.PHONY: nvim-latest
