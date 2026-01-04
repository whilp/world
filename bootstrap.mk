modules += bootstrap
bootstrap_cosmic := $(o)/bootstrap/cosmic
bootstrap_files := $(bootstrap_cosmic)

export PATH := $(o)/bootstrap:$(PATH)

$(bootstrap_cosmic):
	@mkdir -p $(@D)
	@curl -ssLo $@ https://github.com/whilp/cosmopolitan/releases/download/2026.01.04-bc03f9aa8/lua
	@chmod +x $@
	@ln -sf cosmic $(@D)/lua
