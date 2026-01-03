# Home module port plan

## Current state

### lib/home/cook.mk
Uses obsolete patterns:
- `lib_lua_modules`, `lib_dirs`, `lib_libs`, `bins`
- Old paths: `o/%/`, `o/any/`, `o/$(current_platform)/`
- References `$(cosmic)`, `$(lua_bin)` (undefined in new system)
- Bundles 17 3p tools + dotfiles + setup scripts

### 3p tools status

Ported to new module system:
- cosmos
- argparse
- luaunit
- ast-grep

Still using old pattern (need porting):
- biome
- comrak
- delta
- duckdb
- gh
- luacheck
- marksman
- nvim
- rg
- ruff
- shfmt
- sqruff
- stylua
- superhtml
- tree-sitter
- uv

## Phase 1: Port remaining 3p tools

Convert each tool from old pattern:
```makefile
# OLD
tool_version := 3p/tool/version.lua
o/%/tool/archive.tar.gz: $(tool_version) $(fetch)
	$(fetch) $(tool_version) $* $@
o/%/tool/staging/tool: $(tool_version) $(extract) o/%/tool/archive.tar.gz
	$(extract) $(tool_version) $* o/$*/tool/archive.tar.gz o/$*/tool/staging
o/%/tool/bin/tool: $(tool_version) $(install) o/%/tool/staging/tool
	$(install) $(tool_version) $* o/$*/tool bin o/$*/tool/staging/tool
```

To new pattern:
```makefile
# NEW
modules += tool
tool_version := 3p/tool/version.lua
tool_tests := 3p/tool/test_tool.lua
```

The new system auto-derives:
- `tool_staged` = `$(o)/tool/.staged`
- `tool_dir` = `$(o)/tool/.staged` (symlink to extracted files)

### Order of porting
Port in dependency order (tools with no deps first):
1. rg, gh, uv, shfmt, stylua, biome, comrak, delta, ruff, sqruff, superhtml, marksman
2. duckdb, nvim, tree-sitter (may have special needs)
3. luacheck (lua library, different structure)

## Phase 2: Port lib/home

### Convert to new module pattern
```makefile
modules += home
home_srcs := $(wildcard lib/home/*.lua)
home_tests := $(filter lib/home/test_%.lua,$(home_srcs))
home_libs := $(addprefix $(o)/,$(filter-out $(home_tests),$(home_srcs)))
home_bin := $(o)/bin/home
home_files := $(home_bin) $(home_libs)
home_deps := cosmos cosmic <3p-tools...>
```

### Build staging
Use `$(o)/home/.built` for assembling the binary:
```makefile
home_built := $(o)/home/.built

$(home_bin): $(home_libs) <deps>
	@rm -rf $(home_built)
	@mkdir -p $(home_built)/.lua $(@D)
	# ... assembly steps ...
	@$(cp) $(cosmos_lua) $@
	@cd $(home_built) && $(CURDIR)/$(cosmos_zip) -qr $(CURDIR)/$@ .lua
```

### Reference dependencies
- Cosmos: `$(cosmos_lua)`, `$(cosmos_zip)`
- Cosmic: `$(cosmic_bin)`
- 3p tools: `$(tool_dir)` for each tool

### Dotfiles bundling
Keep git ls-files approach but update paths:
```makefile
$(o)/home/dotfiles.zip: $(cosmos_zip)
	git ls-files -z | grep -zZvE '$(home_exclude_pattern)' | xargs -0 $(cosmos_zip) -q $@
```

## Phase 3: Handle platform builds

Home builds platform-specific binaries. Options:

### Option A: Single platform (current host)
```makefile
home_bin := $(o)/bin/home
```
Simplest, matches cosmic pattern.

### Option B: All platforms
```makefile
home_bins := $(foreach p,$(platforms),$(o)/home/$(p)/home)
```
More complex, requires platform-aware staging.

### Recommendation
Start with Option A (single platform). Multi-platform can be added later if needed.

## Checklist

### Phase 1
- [x] Port rg
- [x] Port gh
- [x] Port uv
- [x] Port shfmt
- [x] Port stylua
- [x] Port biome
- [x] Port comrak
- [x] Port delta
- [x] Port ruff
- [x] Port sqruff
- [x] Port superhtml
- [x] Port marksman
- [x] Port duckdb
- [x] Port nvim
- [x] Port tree-sitter
- [x] Port luacheck

### Phase 2
- [x] Convert home to modules pattern
- [x] Update dependency references
- [x] Update build recipe
- [x] Test home binary builds
- [x] Test home binary works

### Phase 3
- [x] Decide on platform approach (single platform for now)
- [ ] Implement multi-platform if needed
