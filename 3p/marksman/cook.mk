marksman_version := 3p/marksman/version.lua
bins += o/%/marksman/bin/marksman


o/%/marksman/download: $(marksman_version) $(fetch)
	$(fetch) $(marksman_version) $* $@

o/%/marksman/bin/marksman: $(marksman_version) $(install) o/%/marksman/download
	$(install) $(marksman_version) $* o/$*/marksman bin o/$*/marksman/download
