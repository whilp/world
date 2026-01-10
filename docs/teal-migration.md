# Teal migration plan

This document outlines the incremental migration from Lua to Teal for comprehensive type checking.

## Goals

1. **Type safety**: Catch type errors at build time, not runtime
2. **Fast iteration**: Quick typechecking during development via `make teal`
3. **Clean releases**: Ship teal-compiled lua in release artifacts
4. **Incremental adoption**: Migrate file-by-file without breaking the build

## Current state

- 107 lua files with `-- teal ignore` comments
- Teal 0.24.8 installed as 3p dependency
- `make teal` target exists (runs `tl check` on each file)
- Checker infrastructure already supports `.tl` extension
- Type declarations in `lib/types/` for external deps
- `tlconfig.lua` at repo root

## Migration phases

### Phase 1: Foundation

Create the type infrastructure needed for migration.

#### PR 1.1: Add tlconfig.lua and type declarations for external deps ✓

**Status: DONE**

1. Created `tlconfig.lua` at repo root with `include_dir = { "lib/types" }`

2. Created `lib/types/` directory with type declarations:
   - `lib/types/cosmo.d.tl` - cosmo module (Fetch, is_main, EncodeJson, etc.)
   - `lib/types/cosmo/unix.d.tl` - unix functions (fork, pipe, exec, etc.)
   - `lib/types/cosmo/path.d.tl` - path functions (join, basename, dirname, etc.)
   - `lib/types/cosmo/getopt.d.tl` - getopt argument parser
   - `lib/types/luaunit.d.tl` - luaunit testing framework
   - `lib/types/argparse.d.tl` - argparse library
   - `lib/types/cosmic/init.d.tl` - cosmic main wrapper
   - `lib/types/cosmic/spawn.d.tl` - process spawning
   - `lib/types/cosmic/walk.d.tl` - directory walking

3. Updated `run-teal.lua` to pass `-I lib/types` to tl check

#### PR 1.2: Build system support for .tl files

1. Add `tl gen` compilation step to build pipeline
2. Update module cook.mk pattern to handle both `.lua` and `.tl` sources
3. Ensure `.tl` files compile to `.lua` in `o/` directory
4. Update release target to use compiled lua

### Phase 2: Core modules

Migrate foundational modules that other code depends on.

#### PR 2.1: Migrate lib/checker/common.lua

- Small, well-defined module (239 lines)
- Used by all checker scripts
- Good test of the migration workflow

Steps:
1. Rename `lib/checker/common.lua` → `lib/checker/common.tl`
2. Add type annotations
3. Update cook.mk to compile it
4. Verify tests pass

#### PR 2.2: Migrate standalone library files

Convert simple, standalone files:
- `lib/file.lua` (file operations)
- `lib/ulid.lua` (ULID generation)
- `lib/utils.lua` (utility functions)
- `lib/platform.lua` (platform detection)

#### PR 2.3: Migrate lib/cosmic/spawn.lua

- Critical module for process spawning
- Well-contained (171 lines)
- Good example of complex types (pipe handles, spawn options)

### Phase 3: Library modules

Migrate lib modules in dependency order.

#### PR 3.1: Migrate lib/environ

- Small module for environment variable handling
- No external dependencies beyond cosmo

#### PR 3.2: Migrate lib/daemonize

- Process daemonization utilities
- Depends on cosmo.unix

#### PR 3.3: Migrate lib/whereami

- Location detection utilities
- Simple, self-contained

#### PR 3.4: Migrate lib/cosmic

Full cosmic module migration:
- `lib/cosmic/init.lua`
- `lib/cosmic/walk.lua`
- `lib/cosmic/help.lua`
- `lib/cosmic/main.lua`
- `lib/cosmic/lfs.lua`

#### PR 3.5: Migrate lib/checker

Complete checker module:
- Already have `common.tl` from PR 2.1
- Migrate test utilities

#### PR 3.6: Migrate lib/build

Build system utilities:
- `build-fetch.lua`
- `build-stage.lua`
- `check-update.lua`
- `reporter.lua`
- `make-help.lua`
- `test-snap.lua`

### Phase 4: Application modules

#### PR 4.1: Migrate lib/work

Work item management (largest module):
- `data.lua` (587 lines)
- `process.lua` (548 lines)
- `api.lua` (479 lines)
- `render.lua` (283 lines)

#### PR 4.2: Migrate lib/skill

Skill modules:
- `hook.lua`
- `pr.lua`
- `pr_comments.lua`
- `bootstrap.lua`

#### PR 4.3: Migrate lib/aerosnap

Window management (296 lines)

#### PR 4.4: Migrate lib/cleanshot

Screenshot utilities (316 lines)

#### PR 4.5: Migrate lib/nvim

Neovim integration:
- `main.lua` (489 lines)
- `init.lua`

#### PR 4.6: Migrate lib/home

Home binary (largest):
- `main.lua` (829 lines)
- `setup/*.lua` (multiple setup modules)
- `mac/*.lua` (macOS defaults)

#### PR 4.7: Migrate lib/claude

Claude API integration

### Phase 5: Third-party runners

#### PR 5.1: Migrate 3p checker runners

- `3p/tl/run-teal.lua`
- `3p/luacheck/run-luacheck.lua`
- `3p/ast-grep/run-astgrep.lua`

### Phase 6: Tests

#### PR 6.1: Decide on test migration strategy

Options:
1. Keep tests as lua (faster iteration)
2. Migrate tests to teal (full type coverage)
3. Hybrid: type-check tests but keep as lua

Recommendation: Start with option 3 (type-check but keep as lua) to get value without churn.

### Phase 7: Cleanup

#### PR 7.1: Remove teal ignore comments

Once all files are migrated or properly typed, remove the `-- teal ignore` comments.

#### PR 7.2: Update documentation

- Update CLAUDE.md with teal patterns
- Add type annotation guidelines

## Special considerations

### Generated files

Two files are auto-generated and should remain lua:
- `lib/emoji/emoji.lua` (35k lines) - generated emoji data
- `lib/symbols/symbols.lua` (60k lines) - generated symbol data

Add these to tlconfig.lua exclusions.

### External dependencies without types

For 3p dependencies without type declarations, create minimal `.d.tl` stubs that declare the public API we use.

### Type strictness

Use Teal's strict mode where possible, but allow `any` types at module boundaries with untyped external code.

### CI integration

`make ci` already runs teal checks. As files are migrated, they will be type-checked automatically.

## Build system changes

### Current flow
```
.lua files → copy to o/ → bundle into binary
```

### Target flow
```
.tl files → tl gen → o/*.lua → bundle into binary
.lua files → copy to o/ → bundle into binary
```

### Makefile updates needed

1. Add pattern rule for `.tl` → `.lua` compilation:
   ```makefile
   $(o)/%.lua: %.tl | $(tl_staged)
       @mkdir -p $(@D)
       @$(tl_gen) $< -o $@
   ```

2. Update vpath to find `.tl` sources
3. Ensure type declarations are available during compilation

## Development workflow

### Type checking during development

```bash
make teal              # check all files
run-test test_foo      # run specific test (includes type check)
```

### Adding a new typed file

1. Create `foo.tl` with type annotations
2. Update `cook.mk` if needed (should auto-detect via wildcards)
3. Run `make teal` to verify types
4. Run tests

### Converting an existing file

1. Rename `foo.lua` → `foo.tl`
2. Add type annotations
3. Remove `-- teal ignore` comment
4. Run `make teal` to verify
5. Run tests

## Metrics

Track migration progress:
- Files remaining with `-- teal ignore`: 107 → 0
- `.tl` files created: 0 → ~100
- Type errors found and fixed during migration

## Timeline notes

This plan is designed for incremental adoption. Each PR should:
- Be self-contained and reviewable
- Not break existing functionality
- Add value immediately (each migrated file gets type checking)

Order can be adjusted based on priorities. Focus first on:
1. High-churn files (more bugs prevented)
2. Core modules (dependencies of many others)
3. Complex logic (where types help most)
