tree_sitter_version := 3p/tree-sitter/version.lua
targets += o/%/tree-sitter/bin/tree-sitter
tests += o/%/tree-sitter/test.ok

o/%/tree-sitter/archive.gz: $(tree_sitter_version) $(fetch)
	$(fetch) $(tree_sitter_version) $* $@

o/%/tree-sitter/staging/tree-sitter: $(tree_sitter_version) $(extract) o/%/tree-sitter/archive.gz
	$(extract) $(tree_sitter_version) $* o/$*/tree-sitter/archive.gz o/$*/tree-sitter/staging

o/%/tree-sitter/bin/tree-sitter: $(tree_sitter_version) $(install) o/%/tree-sitter/staging/tree-sitter
	$(install) $(tree_sitter_version) $* o/$*/tree-sitter bin o/$*/tree-sitter/staging/tree-sitter

o/%/tree-sitter/test.ok: 3p/tree-sitter/test.lua o/%/tree-sitter/bin/tree-sitter
	$< o/$*/tree-sitter && touch $@
