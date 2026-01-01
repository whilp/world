shfmt_version := 3p/shfmt/version.lua
bins += o/%/shfmt/bin/shfmt
tests += o/%/shfmt/test.ok

o/%/shfmt/download: $(shfmt_version) $(fetch)
	$(fetch) $(shfmt_version) $* $@

o/%/shfmt/bin/shfmt: $(shfmt_version) $(install) o/%/shfmt/download
	$(install) $(shfmt_version) $* o/$*/shfmt bin o/$*/shfmt/download

o/%/shfmt/test.ok: 3p/shfmt/test.lua o/%/shfmt/bin/shfmt $(runner)
	TEST_BIN_DIR=o/$*/shfmt $(runner) $< $@
