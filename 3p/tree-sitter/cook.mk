tree_sitter_version := 3p/tree-sitter/version.lua
bins += o/%/tree-sitter/bin/tree-sitter

$(luatest_o)/3p/tree-sitter/test.lua.ok: o/$(current_platform)/tree-sitter/bin/tree-sitter
$(luatest_o)/3p/tree-sitter/test.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/tree-sitter

o/%/tree-sitter/archive.gz: $(tree_sitter_version) $(fetch)
	$(fetch) $(tree_sitter_version) $* $@

o/%/tree-sitter/staging/tree-sitter: $(tree_sitter_version) $(extract) o/%/tree-sitter/archive.gz
	$(extract) $(tree_sitter_version) $* o/$*/tree-sitter/archive.gz o/$*/tree-sitter/staging

o/%/tree-sitter/bin/tree-sitter: $(tree_sitter_version) $(install) o/%/tree-sitter/staging/tree-sitter
	$(install) $(tree_sitter_version) $* o/$*/tree-sitter bin o/$*/tree-sitter/staging/tree-sitter
