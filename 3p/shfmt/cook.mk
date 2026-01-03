shfmt_version := 3p/shfmt/version.lua
bins += o/%/shfmt/bin/shfmt


o/%/shfmt/download: $(shfmt_version) $(fetch)
	$(fetch) $(shfmt_version) $* $@

o/%/shfmt/bin/shfmt: $(shfmt_version) $(install) o/%/shfmt/download
	$(install) $(shfmt_version) $* o/$*/shfmt bin o/$*/shfmt/download
