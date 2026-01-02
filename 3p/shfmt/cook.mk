shfmt_version := 3p/shfmt/version.lua
bins += o/%/shfmt/bin/shfmt

o/luatest/3p/shfmt/test.lua.ok: o/$(current_platform)/shfmt/bin/shfmt
o/luatest/3p/shfmt/test.lua.ok: TEST_ENV = TEST_BIN_DIR=o/$(current_platform)/shfmt

o/%/shfmt/download: $(shfmt_version) $(fetch)
	$(fetch) $(shfmt_version) $* $@

o/%/shfmt/bin/shfmt: $(shfmt_version) $(install) o/%/shfmt/download
	$(install) $(shfmt_version) $* o/$*/shfmt bin o/$*/shfmt/download
