# Teal migration plan

This document outlines the incremental migration from Lua to Teal for comprehensive type checking.

## Goals

1. **Type safety**: Catch type errors at build time, not runtime
2. **Fast iteration**: Quick typechecking during development via `make teal`
3. **Clean releases**: Ship teal-compiled lua in release artifacts
4. **Incremental adoption**: Migrate file-by-file without breaking the build

## Current state

- 22 lua files with `-- teal ignore` comments (down from 107)
- 85 `.tl` files migrated:
  - `lib/checker/common.tl`
  - `lib/ulid.tl`
  - `lib/utils.tl`
  - `lib/platform.tl`
  - `lib/file.tl`
  - `lib/cosmic/spawn.tl`
  - `lib/cosmic/init.tl`
  - `lib/cosmic/walk.tl`
  - `lib/cosmic/help.tl`
  - `lib/cosmic/main.tl`
  - `lib/cosmic/lfs.tl`
  - `lib/environ/init.tl`
  - `lib/daemonize/init.tl`
  - `lib/whereami/init.tl`
  - `lib/build/build-fetch.tl`
  - `lib/build/build-stage.tl`
  - `lib/build/check-update.tl`
  - `lib/build/reporter.tl`
  - `lib/build/make-help.tl`
  - `lib/build/test-snap.tl`
  - `lib/aerosnap/init.tl`
  - `lib/cleanshot/init.tl`
  - `lib/claude/main.tl`
  - `lib/nvim/main.tl`
  - `lib/work/data.tl`
  - `lib/work/process.tl`
  - `lib/work/api.tl`
  - `lib/work/render.tl`
  - `lib/work/config.tl`
  - `lib/work/validate.tl`
  - `lib/work/store.tl`
  - `lib/skill/init.tl`
  - `lib/skill/hook.tl`
  - `lib/skill/pr.tl`
  - `lib/skill/pr_comments.tl`
  - `lib/skill/bootstrap.tl`
  - `lib/home/main.tl`
  - `lib/home/gen-manifest.tl`
  - `lib/home/gen-platforms.tl`
  - `lib/home/setup/*.tl` (13 files)
  - `lib/home/mac/*.tl` (30 files)
  - `3p/tl/run-teal.tl`
  - `3p/luacheck/run-luacheck.tl`
  - `3p/ast-grep/run-astgrep.tl`
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

#### PR 1.2: Build system support for .tl files ✓

**Status: DONE** (completed as part of PR 2.1)

1. Added `tl gen` compilation step via pattern rule `o/teal/lib/%.lua: lib/%.tl`
2. Updated module cook.mk patterns to handle both `.lua` and `.tl` sources
3. `.tl` files compile to `o/teal/lib/` directory via `tl gen -o`
4. Added `o/teal/lib` to `LUA_PATH` for runtime module resolution

#### PR 1.3: Add ast-grep support for .tl files ✓

**Status: DONE**

- Added `.tl` to `supported_extensions` in `run-astgrep.lua`
- Added `**/*.tl` to `languageGlobs.lua` in `sgconfig.yml`
- Existing lua rules work correctly on teal files (tested with `lib/checker/common.tl`)

### Phase 2: Core modules

Migrate foundational modules that other code depends on.

#### PR 2.1: Migrate lib/checker/common.lua ✓

**Status: DONE**

- Converted `lib/checker/common.lua` to `lib/checker/common.tl` with full type annotations
- Defined record types: `CheckResult`, `CheckPatterns`, `CategorizedResults`, `StatusIcons`
- Updated `lib/checker/cook.mk` to compile `.tl` files to `o/teal/lib/`
- Added `o/teal/lib` to `LUA_PATH` in Makefile for compiled module resolution
- Added `checker_files` as dependency to teal checker rule
- Added pattern rule for `o/teal/lib/%.lua` compilation via `tl gen -o`

#### PR 2.2: Migrate lib/ulid.lua ✓

**Status: DONE**

- Converted `lib/ulid.lua` to `lib/ulid.tl` with full type annotations
- Defined `DecodedUlid` record type for decode() return value
- Added `lib_srcs` to `lib/cook.mk` to include standalone lib files in teal checking
- Updated `tl-gen.lua` to use getopt for argument parsing (-o flag)

#### PR 2.3: Migrate remaining standalone library files ✓

**Status: DONE** (migrated in parallel with PR 2.4 using agent strategy)

- Converted `lib/utils.lua` to `lib/utils.tl` with generic type annotations
- Converted `lib/platform.lua` to `lib/platform.tl` with const maps
- Converted `lib/file.lua` to `lib/file.tl` with posix type declarations
- Added type declarations:
  - `lib/types/version.d.tl`
  - `lib/types/posix/sys/stat.d.tl`
  - `lib/types/posix/dirent.d.tl`
  - `lib/types/posix/unistd.d.tl`

#### PR 2.4: Migrate lib/cosmic/spawn.lua ✓

**Status: DONE** (migrated in parallel with PR 2.3 using agent strategy)

- Converted `lib/cosmic/spawn.lua` to `lib/cosmic/spawn.tl`
- Defined record types: `Pipe`, `SpawnHandle`, `SpawnOpts`, `SpawnModule`
- Complex types for pipe handles with read/write/close methods

### Phase 3: Library modules

Migrate lib modules in dependency order.

#### PR 3.1: Migrate small standalone modules ✓

**Status: DONE** (migrated in parallel using agent strategy)

- Converted `lib/environ/init.lua` to `lib/environ/init.tl`
  - Added `Environ` record type with metamethods for env var access
- Converted `lib/daemonize/init.lua` to `lib/daemonize/init.tl`
  - Added types for pidfile, lock, and output redirection functions
  - Extended `lib/types/cosmo/unix.d.tl` with dup2 and fcntl signatures
- Converted `lib/whereami/init.lua` to `lib/whereami/init.tl`
  - Added `EnvFunc` type alias and union types for cached state

#### PR 3.2: Migrate lib/cosmic ✓

**Status: DONE** (migrated in parallel using agent strategy)

Full cosmic module migration:
- Converted `lib/cosmic/init.lua` to `lib/cosmic/init.tl`
  - Added `Env` record type with stdout/stderr FILE handles
  - Added `MainFn` type alias and `cosmic` record type
- Converted `lib/cosmic/walk.lua` to `lib/cosmic/walk.tl`
  - Added `Stat`, `DirHandle`, `FileInfo` record types
  - Added `Visitor` type alias for walk callbacks
  - Generic type `walk<T>` for context accumulator
- Converted `lib/cosmic/help.lua` to `lib/cosmic/help.tl`
  - Added `ModuleInfo` and `HelpModule` record types
- Converted `lib/cosmic/main.lua` to `lib/cosmic/main.tl`
  - Added `Opts` record with union types for help and stdin
  - Added `LongOpt` type alias, `SkillModule` record
  - Updated `cosmic/cook.mk` to reference compiled output
- Converted `lib/cosmic/lfs.lua` to `lib/cosmic/lfs.tl`
  - Added `Attrs` record for file attributes
  - Added `lfs` record with function type declarations
- Changed `tlconfig.lua` gen_target from "5.1" to "5.4" for native bitwise operators

#### PR 3.5: Migrate lib/checker

Complete checker module:
- Already have `common.tl` from PR 2.1
- Migrate test utilities

#### PR 3.6: Migrate lib/build ✓

**Status: DONE** (migrated in parallel using agent strategy)

Build system utilities - special handling required:
- Build module has bootstrap dependency (needs itself to fetch/stage teal)
- Solution: keep both `.lua` and `.tl` files for bootstrap-critical modules
- `.lua` files used for bootstrap (copied to o/bin/)
- `.tl` files used for type checking via `make teal`

Migrated files:
- `lib/build/build-fetch.tl` - fetch archive downloads
- `lib/build/build-stage.tl` - archive extraction and staging
- `lib/build/check-update.tl` - GitHub release version checking
- `lib/build/reporter.tl` - test/check result reporting
- `lib/build/make-help.tl` - Makefile help generation
- `lib/build/test-snap.tl` - snapshot testing utility

### Phase 4: Application modules

#### PR 4.1: Migrate independent app modules ✓

**Status: DONE** (migrated in parallel using agent strategy)

Independent application modules:
- `lib/aerosnap/init.tl` - window management with SQLite types
- `lib/cleanshot/init.tl` - screenshot utility with Flags record
- `lib/claude/main.tl` - Claude binary launcher
- `lib/nvim/main.tl` - Neovim wrapper with daemon support

Added type declarations:
- `lib/types/cosmo/lsqlite3.d.tl` - SQLite3 bindings
- `lib/types/daemonize.d.tl` - daemon utilities
- `lib/types/whereami.d.tl` - executable path discovery

#### PR 4.2: Migrate lib/work ✓

**Status: DONE** (migrated in parallel using agent strategy)

Work item management (largest module):
- `lib/work/data.tl` - core data types (WorkItem, WorkStore records)
- `lib/work/process.tl` - work item processing
- `lib/work/api.tl` - API layer
- `lib/work/render.tl` - output rendering
- `lib/work/config.tl` - configuration
- `lib/work/validate.tl` - validation
- `lib/work/store.tl` - storage layer

Added type declarations:
- `lib/types/serpent.d.tl` - serialization
- `lib/types/ulid.d.tl` - ULID types
- `lib/types/posix/fcntl.d.tl` - file control
- `lib/types/posix/glob.d.tl` - glob patterns
- `lib/types/posix/stdlib.d.tl` - stdlib functions
- `lib/types/posix/signal.d.tl` - signals
- `lib/types/posix/init.d.tl` - posix init

#### PR 4.3: Migrate lib/skill ✓

**Status: DONE** (migrated in parallel using agent strategy)

Skill modules:
- `lib/skill/init.tl` - module metadata
- `lib/skill/hook.tl` - hook dispatcher with flexible input/output types
- `lib/skill/pr.tl` - GitHub PR operations
- `lib/skill/pr_comments.tl` - PR comment handling
- `lib/skill/bootstrap.tl` - environment bootstrap

Fixed issues:
- Updated LUA_PATH to include both `o/teal/lib/` and `o/lib/` for module resolution
- Made HookInput/HookOutput flexible with `{string:any}` to support arbitrary fields
- Fixed github_request to return string errors instead of tables

#### PR 4.4: Migrate lib/home ✓

**Status: DONE** (migrated in parallel using agent strategy)

Home binary (largest module with 46 files):
- `lib/home/main.tl` - main home binary with type-annotated options and commands
- `lib/home/gen-manifest.tl` - manifest generation
- `lib/home/gen-platforms.tl` - platform generation
- `lib/home/setup/*.tl` (13 files) - setup modules (env, shell, git, nvim, claude, ai, backup, extras, codespace, validate, verify, util, setup)
- `lib/home/mac/*.tl` (30 files) - macOS defaults modules

Key type definitions:
- `ArchMap`, `PlatformMap` type aliases for manifest handling
- `ParsedArgs`, `SetupOpts`, `MacOpts` records for options
- `SetupModule`, `MacModule` records for dynamic module loading
- `Env` record for environment configuration

### Phase 5: Third-party runners

#### PR 5.1: Migrate 3p checker runners ✓

**Status: DONE** (migrated in parallel using agent strategy)

- `3p/tl/run-teal.tl` - teal checker runner with Issue record type
- `3p/luacheck/run-luacheck.tl` - luacheck runner with Issue record type
- `3p/ast-grep/run-astgrep.tl` - ast-grep runner with AstGrepResult, Issue types

Updated Makefile with vpath for .tl files and pattern rule for compiling to o/bin/.

### Phase 6: Tests

#### PR 6.1: Decide on test migration strategy

Options:
1. Keep tests as lua (faster iteration)
2. Migrate tests to teal (full type coverage)
3. Hybrid: type-check tests but keep as lua

Recommendation: Start with option 3 (type-check but keep as lua) to get value without churn.

### Phase 7: Cleanup

#### PR 7.1: Update documentation

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

### Makefile updates (done)

1. Define type declaration files:
   ```makefile
   types_files := $(wildcard lib/types/*.d.tl lib/types/**/*.d.tl)
   ```

2. Pattern rule for `.tl` → `.lua` compilation:
   ```makefile
   $(o)/%.lua: %.tl $(types_files) $(tl_files) $(bootstrap_files) | $(tl_staged)
       @mkdir -p $(@D)
       @$(tl_gen) $< -o $@
   ```
   Note: `$(tl_files)` and `$(bootstrap_files)` are required for parallel builds (`make -j4`)

3. vpath updated to find `.tl` sources

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

1. Remove `-- teal ignore` comment (enables type checking immediately)
2. Rename `foo.lua` → `foo.tl`
3. Add type annotations
4. Run `make teal` to verify
5. Run tests

Note: Remove the `-- teal ignore` directive as part of each file migration, not as a separate cleanup phase. This ensures type checking is enabled immediately for migrated files.

## Metrics

Track migration progress:
- Files remaining with `-- teal ignore`: 107 → 0
- `.tl` files created: 0 → ~100
- Type errors found and fixed during migration

## Parallel agent strategy

Independent files can be migrated concurrently using parallel agents. This approach was validated in PR 2.3/2.4 where 4 files were migrated simultaneously.

### How it works

1. Identify files with no dependencies on each other
2. Spawn one agent per file with migration instructions
3. Each agent:
   - Reads the source file and reference `.tl` files (e.g., `lib/ulid.tl`)
   - Creates any needed type declarations in `lib/types/`
   - Converts the file to `.tl` with proper annotations
   - Updates `cook.mk` if needed
   - Runs `make teal` to verify
4. After all agents complete, run `make test` to validate

### Agent opportunities by phase

**Phase 3: Library modules** (6 parallel batches possible)

Batch 3.1 - Small standalone modules ✓ (completed with 3 parallel agents):
- `lib/environ/init.tl`
- `lib/daemonize/init.tl`
- `lib/whereami/init.tl`

Batch 3.2 - Cosmic module ✓ (completed with 5 parallel agents):
- `lib/cosmic/init.tl`
- `lib/cosmic/walk.tl`
- `lib/cosmic/help.tl`
- `lib/cosmic/main.tl`
- `lib/cosmic/lfs.tl`

Batch 3.3 - Build utilities ✓ (completed with 6 parallel agents):
- `lib/build/build-fetch.tl`
- `lib/build/build-stage.tl`
- `lib/build/check-update.tl`
- `lib/build/reporter.tl`
- `lib/build/make-help.tl`
- `lib/build/test-snap.tl`
- Note: Build module keeps both .lua and .tl due to bootstrap dependency

**Phase 4: Application modules** (can run some in parallel)

Batch 4.1 - Independent app modules ✓ (completed with 4 parallel agents):
- `lib/aerosnap/init.tl`
- `lib/cleanshot/init.tl`
- `lib/claude/main.tl`
- `lib/nvim/main.tl`

Batch 4.2 - Work module ✓ (completed with 7 parallel agents):
- `lib/work/data.tl` (core types)
- `lib/work/process.tl`
- `lib/work/api.tl`
- `lib/work/render.tl`
- `lib/work/config.tl`
- `lib/work/validate.tl`
- `lib/work/store.tl`

Batch 4.3 - Skill module ✓ (completed with 5 parallel agents):
- `lib/skill/init.tl`
- `lib/skill/hook.tl`
- `lib/skill/pr.tl`
- `lib/skill/pr_comments.tl`
- `lib/skill/bootstrap.tl`

Batch 4.4 - Home module ✓ (completed with 4 parallel agents):
- `lib/home/main.tl`
- `lib/home/setup/*.tl` (13 files)
- `lib/home/mac/*.tl` (30 files)
- `lib/home/gen-*.tl` (2 files)

**Phase 5: 3p runners** ✓ (completed with 3 parallel agents):
- `3p/tl/run-teal.tl`
- `3p/luacheck/run-luacheck.tl`
- `3p/ast-grep/run-astgrep.tl`

### Best practices for agent prompts

Each agent prompt should include:
1. The specific file(s) to migrate
2. Reference to existing `.tl` files for syntax patterns
3. Which type declarations may need to be created
4. Instructions to run `make teal` to verify
5. Note about updating `cook.mk` if needed

### Estimated parallelization

| Phase | Files | Max parallel | Batches |
|-------|-------|--------------|---------|
| 3     | 14    | 6            | 3       |
| 4     | 20+   | 4-6          | 4-5     |
| 5     | 3     | 3            | 1       |

Total: ~37 files across 8-9 batches instead of 37 sequential PRs.

## Execution notes

This plan is designed for incremental adoption. Each PR should:
- Be self-contained and reviewable
- Not break existing functionality
- Add value immediately (each migrated file gets type checking)

Order can be adjusted based on priorities. Focus first on:
1. High-churn files (more bugs prevented)
2. Core modules (dependencies of many others)
3. Complex logic (where types help most)
