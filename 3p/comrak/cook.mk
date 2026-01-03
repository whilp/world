comrak_version := 3p/comrak/version.lua
bins += o/%/comrak/bin/comrak


o/%/comrak/download: $(comrak_version) $(fetch)
	$(fetch) $(comrak_version) $* $@

o/%/comrak/bin/comrak: $(comrak_version) $(install) o/%/comrak/download
	$(install) $(comrak_version) $* o/$*/comrak bin o/$*/comrak/download
