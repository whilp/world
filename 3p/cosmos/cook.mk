cosmos_url := https://github.com/jart/cosmopolitan/releases/download/4.0.2/cosmos-4.0.2.zip
cosmos_sha256 := 494ecbd87c2f2f622f91066d4fe5d9ffc1aaaa13de02db1714dd84d014ed398f
cosmos_dir := $(3p)/cosmos
cosmos_zip := $(cosmos_dir)/cosmos.zip
cosmos_bin := $(cosmos_dir)/bin/make

$(cosmos_bin): $(cosmos_zip)
	$(unzip) -o $< -d $(cosmos_dir)

$(cosmos_zip): | $(cosmos_dir)
	$(curl) -o $@ $(cosmos_url)
	cd $(dir $@) && echo "$(cosmos_sha256)  $(notdir $@)" | $(sha256sum) -c

$(cosmos_dir):
	mkdir -p $@
