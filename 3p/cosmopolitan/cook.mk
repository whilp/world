cosmopolitan_url = https://github.com/jart/cosmopolitan/releases/download/4.0.2/cosmopolitan-4.0.2.tar.gz
cosmopolitan_sha256 = e466106b18064e0c996ef64d261133af867bccd921ad14e54975d89aa17a8717
cosmopolitan_dir := $(3p)/cosmopolitan
cosmopolitan_tarball := $(cosmopolitan_dir)/cosmopolitan.tar.gz
cosmopolitan_src := $(cosmopolitan_dir)/cosmopolitan-4.0.2/Makefile

$(cosmopolitan_src): $(cosmopolitan_tarball)
	cd $(cosmopolitan_dir) && $(tar) -xzf $(notdir $<)

$(cosmopolitan_tarball): | $(cosmopolitan_dir)
	$(curl) -o $@ $(cosmopolitan_url)
	cd $(dir $@) && echo "$(cosmopolitan_sha256)  $(notdir $@)" | $(sha256sum) -c

$(cosmopolitan_dir):
	mkdir -p $@
