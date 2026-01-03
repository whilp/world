modules += bootstrap
bootstrap_cosmic := $(o)/bootstrap/cosmic
bootstrap_files := $(bootstrap_cosmic)

export PATH := $(o)/bootstrap:$(PATH)

$(bootstrap_cosmic):
	@mkdir -p $(@D)
	curl -ssLo $@ https://github.com/whilp/world/releases/download/home-2026-01-02-75caba6/cosmic-lua
	@chmod +x $@
	ln -sf cosmic $(@D)/lua
