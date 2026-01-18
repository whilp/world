modules += bootstrap
bootstrap_cosmic := $(o)/bootstrap/cosmic
bootstrap_files := $(bootstrap_cosmic)

export PATH := $(o)/bootstrap:$(PATH)

bin/cosmic-lua: bin/cosmic
	@bin/cosmic --version >/dev/null 2>&1 || true

$(bootstrap_cosmic): bin/cosmic-lua
	@mkdir -p $(@D)
	@cp bin/cosmic-lua $@
	@chmod +x $@
	@ln -sf cosmic $(@D)/lua
