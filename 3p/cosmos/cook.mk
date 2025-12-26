# cosmos binaries from whilp/cosmopolitan fork
cosmos_dir := $(3p)/cosmos
cosmos_version := 2025.12.26-564e3b5b9
cosmos_url := https://github.com/whilp/cosmopolitan/releases/download/$(cosmos_version)

$(eval $(call download_binary_rule,cosmos,lua,$(cosmos_url)/lua,089ac9689fdd5d522ecfd9940497528f0e5fa6f39945f5b1ffc8a60541bdb971))
$(eval $(call download_binary_rule,cosmos,zip,$(cosmos_url)/zip,12bcc7b92c1d538f784bba74e00719230922074b49e777d608e6d8727d2ef0e1))
$(eval $(call download_binary_rule,cosmos,unzip,$(cosmos_url)/unzip,c52a0bcaafda0484353677830895ef74e019c1b377a7831bf1422936769f34dd))
$(eval $(call download_binary_rule,cosmos,make,$(cosmos_url)/make,b60bb21aee7c91a9f64db750859896900aa2a70262675ff629d0512f29f40268))

cosmos_bin := $(cosmos_make_bin)

cosmos: $(cosmos_lua_bin) $(cosmos_zip_bin) $(cosmos_unzip_bin) $(cosmos_make_bin)
.PHONY: cosmos

$(cosmos_dir)/bin:
	mkdir -p $@
