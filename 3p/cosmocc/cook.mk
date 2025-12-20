cosmocc_url := https://github.com/jart/cosmopolitan/releases/download/4.0.2/cosmocc-4.0.2.zip
cosmocc_sha256 := 85b8c37a406d862e656ad4ec14be9f6ce474c1b436b9615e91a55208aced3f44
cosmocc_dir := $(3p)/cosmocc
cosmocc_zip := $(cosmocc_dir)/cosmocc.zip
cosmocc_bin := $(cosmocc_dir)/bin/cosmocc

$(cosmocc_bin): $(cosmocc_zip)
	$(unzip) -o $< -d $(cosmocc_dir)

$(cosmocc_zip): | $(cosmocc_dir)
	$(curl) -o $@ $(cosmocc_url)
	cd $(dir $@) && echo "$(cosmocc_sha256)  $(notdir $@)" | $(sha256sum) -c

$(cosmocc_dir):
	mkdir -p $@
