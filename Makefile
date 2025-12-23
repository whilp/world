include 3p/cook.mk
include 3p/luaunit/cook.mk
include 3p/luacheck/cook.mk
include 3p/cosmopolitan/cook.mk
include 3p/make/cook.mk
include 3p/lua/cook.mk
include 3p/nvim/cook.mk
include 3p/gh/cook.mk
include 3p/delta/cook.mk
include 3p/rg/cook.mk
include 3p/duckdb/cook.mk
include 3p/tree-sitter/cook.mk
include 3p/ast-grep/cook.mk
include 3p/biome/cook.mk
include 3p/comrak/cook.mk
include 3p/marksman/cook.mk
include 3p/ruff/cook.mk
include 3p/shfmt/cook.mk
include 3p/sqruff/cook.mk
include 3p/stylua/cook.mk
include 3p/superhtml/cook.mk
include 3p/uv/cook.mk
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

# Aggregate all binary extraction markers
all_binaries := \
	$(nvim_binaries) \
	$(gh_binaries) \
	$(delta_binaries) \
	$(rg_binaries) \
	$(duckdb_binaries) \
	$(tree_sitter_binaries) \
	$(ast_grep_binaries) \
	$(biome_binaries) \
	$(comrak_binaries) \
	$(marksman_binaries) \
	$(ruff_binaries) \
	$(shfmt_binaries) \
	$(sqruff_binaries) \
	$(stylua_binaries) \
	$(superhtml_binaries) \
	$(uv_binaries)

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
