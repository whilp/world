marksman_version := 3p/marksman/version.lua
bins += o/%/marksman/bin/marksman
tests += o/%/marksman/test.ok

o/%/marksman/download: $(marksman_version) $(fetch)
	$(fetch) $(marksman_version) $* $@

o/%/marksman/bin/marksman: $(marksman_version) $(install) o/%/marksman/download
	$(install) $(marksman_version) $* o/$*/marksman bin o/$*/marksman/download

o/%/marksman/test.ok: 3p/marksman/test.lua o/%/marksman/bin/marksman $(runner)
	TEST_BIN_DIR=o/$*/marksman $(runner) $< $@
