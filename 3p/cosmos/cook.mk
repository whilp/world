cosmos_dir := $(3p)/cosmos
cosmos_version := 4.0.2
cosmos_url := https://cosmo.zip/pub/cosmos/v/$(cosmos_version)/bin
$(eval $(call download_binary_rule,cosmos,zip,$(cosmos_url)/zip,6741fead27d6431939921e75f86b42f1dc3e3a2ec8a82b31490747185c7b9349))
$(eval $(call download_binary_rule,cosmos,unzip,$(cosmos_url)/unzip,e96740f10b773c158d471027285cc694d410f9387bc206c93e5bd075238fead6))
$(eval $(call download_binary_rule,cosmos,make,$(cosmos_url)/make,15317323a22b13dfd213bce47a39d302e74065f42199deb17c85852c04934fd9))
cosmos_bin := $(cosmos_make_bin)

$(cosmos_dir)/bin:
	mkdir -p $@
