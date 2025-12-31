shfmt_version := 3p/shfmt/version.lua
targets += o/%/shfmt/bin/shfmt
tests += o/%/shfmt/test.ok

o/%/shfmt/download: $(shfmt_version) $(fetch)
	$(fetch) $(shfmt_version) $* $@

o/%/shfmt/bin/shfmt: $(shfmt_version) $(install) o/%/shfmt/download
	$(install) $(shfmt_version) $* o/$*/shfmt/download o/$*/shfmt

o/%/shfmt/test.ok: 3p/shfmt/test.lua o/%/shfmt/bin/shfmt
	$< o/$*/shfmt && touch $@
