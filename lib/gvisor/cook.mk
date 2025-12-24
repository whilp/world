# gVisor-compatible assets
# Produces assimilated (native ELF) binaries that work in gVisor sandboxes
# like claude.ai/code

gvisor_dir := results/gvisor
gvisor_bin := $(gvisor_dir)/bin
gvisor_lib := $(gvisor_dir)/lib/lua

# cosmos binaries to include (will be assimilated)
gvisor_cosmos_bins := make curl zip unzip

# assimilate binary from cosmos
cosmos_assimilate := $(cosmos_dir)/bin/assimilate

$(cosmos_assimilate): $(cosmos_bin)

# assimilated lua binary (-c = clobber, no .bak file)
$(gvisor_bin)/lua: $(lua_bin) $(cosmos_assimilate) | $(gvisor_bin)
	cp $< $@
	$(cosmos_assimilate) -ce $@

# assimilated cosmos binaries
define gvisor_cosmos_rule
$(gvisor_bin)/$(1): $(cosmos_dir)/bin/$(1) $(cosmos_assimilate) | $(gvisor_bin)
	cp $$< $$@
	$(cosmos_assimilate) -ce $$@
endef

$(foreach bin,$(gvisor_cosmos_bins),$(eval $(call gvisor_cosmos_rule,$(bin))))

# lua modules (from luaunit/luacheck)
$(gvisor_lib)/luaunit.lua: $(luaunit_lua_dir)/luaunit.lua | $(gvisor_lib)
	cp $< $@

$(gvisor_lib)/argparse.lua: $(luacheck_lua_dir)/argparse.lua | $(gvisor_lib)
	cp $< $@

$(gvisor_lib)/lfs.lua: $(luacheck_lua_dir)/lfs.lua | $(gvisor_lib)
	cp $< $@

$(gvisor_lib)/luacheck: $(luacheck_lua_dir)/bin/luacheck | $(gvisor_lib)
	cp -r $(luacheck_lua_dir)/luacheck $@

$(gvisor_lib)/bin/luacheck: $(luacheck_lua_dir)/bin/luacheck | $(gvisor_lib)/bin
	cp $< $@

# env script for setting up PATH and LUA_PATH
$(gvisor_dir)/env.sh: | $(gvisor_dir)
	@echo '#!/bin/sh' > $@
	@echo '# source this file to set up gvisor-tools environment' >> $@
	@echo 'GVISOR_TOOLS_DIR="$$(cd "$$(dirname "$$0")" && pwd)"' >> $@
	@echo 'export PATH="$$GVISOR_TOOLS_DIR/bin:$$PATH"' >> $@
	@echo 'export LUA_PATH="$$GVISOR_TOOLS_DIR/lib/lua/?.lua;$$GVISOR_TOOLS_DIR/lib/lua/?/init.lua;;"' >> $@

# directories
$(gvisor_dir) $(gvisor_bin) $(gvisor_lib) $(gvisor_lib)/bin:
	mkdir -p $@

# all gvisor assets
gvisor_bins := $(gvisor_bin)/lua $(foreach bin,$(gvisor_cosmos_bins),$(gvisor_bin)/$(bin))
gvisor_libs := $(gvisor_lib)/luaunit.lua $(gvisor_lib)/argparse.lua $(gvisor_lib)/lfs.lua $(gvisor_lib)/luacheck $(gvisor_lib)/bin/luacheck

# tarball
GVISOR_VERSION ?= $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")
gvisor_tarball := results/assimilated-linux-x86_64-$(GVISOR_VERSION).tar.gz

$(gvisor_tarball): $(gvisor_bins) $(gvisor_libs) $(gvisor_dir)/env.sh
	cd results && tar -czf $(notdir $@) gvisor

gvisor: $(gvisor_tarball)
	@echo "Built: $<"
	@echo "Extract and source env.sh to use"

clean-gvisor:
	rm -rf $(gvisor_dir) results/assimilated-*.tar.gz

.PHONY: gvisor clean-gvisor
