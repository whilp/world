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
include src/home/cook.mk
include src/test.mk

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
check:
	@if [ "$$(uname)" = "Darwin" ]; then \
		PLATFORM="darwin-arm64"; \
	elif [ "$$(uname -m)" = "aarch64" ]; then \
		PLATFORM="linux-arm64"; \
	else \
		PLATFORM="linux-x86_64"; \
	fi; \
	SG="$(3p)/ast-grep/$$PLATFORM/sg"; \
	if [ ! -x "$$SG" ]; then \
		echo "ast-grep not found at $$SG, building..."; \
		$(MAKE) $(3p)/ast-grep/$$PLATFORM/.extracted; \
	fi; \
	$$SG scan --color always
	@echo ""
	@echo "Running luacheck..."
	@if [ ! -x results/bin/lua ]; then \
		echo "lua binary not found, building..."; \
		$(MAKE) lua; \
	fi
	@results/bin/lua /zip/.lua/bin/luacheck \
		.config \
		.local/lib/lua \
		.github \
		src \
		--exclude-files '.claude/skills/lua/templates/*.lua' \
		|| true

.PHONY: build clean check
