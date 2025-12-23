include 3p/cook.mk
include 3p/luaunit/cook.mk
include 3p/luacheck/cook.mk
include 3p/cosmopolitan/cook.mk
include 3p/make/cook.mk
include 3p/lua/cook.mk
include lib/home/cook.mk
include lib/test.mk

UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)
ifeq ($(UNAME_S),Darwin)
  PLATFORM := darwin-arm64
else ifeq ($(UNAME_M),aarch64)
  PLATFORM := linux-arm64
else
  PLATFORM := linux-x86_64
endif

ast_grep_bin := $(ast_grep_dir)/$(PLATFORM)/ast-grep
ast_grep_extracted := $(ast_grep_dir)/$(PLATFORM)/.extracted

build: lua

clean:
	rm -rf o results

# all_binaries is now defined in 3p/cook.mk

# Platform-specific binaries zips
results/binaries-darwin-arm64.zip: private .UNVEIL = \
	r:$(3p) \
	rx:$(cosmos_zip_bin) \
	rwc:results \
	rw:/dev/null
results/binaries-darwin-arm64.zip: $(all_binaries) $(cosmos_zip_bin) | results
	cd $(3p) && \
		find . -path '*/darwin-arm64/*' -type f ! -name '.extracted' | \
		$(cosmos_zip_bin) -q $(CURDIR)/$@ -@

results/binaries-linux-arm64.zip: private .UNVEIL = \
	r:$(3p) \
	rx:$(cosmos_zip_bin) \
	rwc:results \
	rw:/dev/null
results/binaries-linux-arm64.zip: $(all_binaries) $(cosmos_zip_bin) | results
	cd $(3p) && \
		find . -path '*/linux-arm64/*' -type f ! -name '.extracted' | \
		$(cosmos_zip_bin) -q $(CURDIR)/$@ -@

results/binaries-linux-x86_64.zip: private .UNVEIL = \
	r:$(3p) \
	rx:$(cosmos_zip_bin) \
	rwc:results \
	rw:/dev/null
results/binaries-linux-x86_64.zip: $(all_binaries) $(cosmos_zip_bin) | results
	cd $(3p) && \
		find . -path '*/linux-x86_64/*' -type f ! -name '.extracted' | \
		$(cosmos_zip_bin) -q $(CURDIR)/$@ -@

results/bin:
	mkdir -p $@

results:
	mkdir -p $@

check: private .UNVEIL = \
	r:$(CURDIR) \
	rx:$(3p)/ast-grep \
	rx:results/bin \
	rw:/dev/null
check: $(ast_grep_extracted) lua
	$(ast_grep_bin) scan --color always
	@echo ""
	@echo "Running luacheck..."
	@$(lua_bin) /zip/.lua/bin/luacheck \
		.config \
		lib \
		.github \
		--exclude-files '.claude/skills/lua/templates/*.lua' \
		--exclude-files '.config/nvim/**/*.lua' \
		--exclude-files '.config/hammerspoon/**/*.lua'

.PHONY: build clean check
