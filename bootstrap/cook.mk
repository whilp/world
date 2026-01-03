modules += bootstrap
bootstrap_bin := o/bootstrap/bin
bootstrap_cosmic := $(bootstrap_bin)/cosmic
bootstrap_files := $(bootstrap_cosmic)
bootstrap_tests :=
bootstrap_deps :=
bootstrap_archives :=

export PATH := $(CURDIR)/$(bootstrap_bin):$(PATH)

$(bootstrap_cosmic):
	@mkdir -p $(@D)
	curl -ssLo $@ https://github.com/whilp/world/releases/download/home-2026-01-02-75caba6/cosmic-lua
	@chmod +x $@
	ln -sf cosmic $(@D)/lua
