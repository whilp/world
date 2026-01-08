modules += make
make_files := $(o)/bin/make-help.lua $(o)/bin/make-check-help.lua
make_srcs := lib/make/help.lua lib/make/check-help.lua

$(o)/bin/make-help.lua: lib/make/help.lua
	@mkdir -p $(@D)
	@cp $< $@
	@chmod +x $@

$(o)/bin/make-check-help.lua: lib/make/check-help.lua
	@mkdir -p $(@D)
	@cp $< $@
	@chmod +x $@
