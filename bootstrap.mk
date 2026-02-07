modules += bootstrap
bootstrap_cosmic := $(o)/bootstrap/cosmic
bootstrap_files := $(bootstrap_cosmic)
bootstrap_url := https://github.com/whilp/cosmic/releases/download/2026-02-06-c7537ca/cosmic-lua

export PATH := $(CURDIR)/$(o)/bootstrap:$(PATH)

$(bootstrap_cosmic):
	@mkdir -p $(@D)
	@curl -fsSL -o $@ $(bootstrap_url)
	@chmod +x $@
	@ln -sf cosmic $(@D)/lua
