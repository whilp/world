# Shimlink improvement plan

After thoroughly reviewing the shimlink script (.local/bin/shimlink:1-846) and its dependencies (platform.lua:1-184, version.lua:1-162), I've identified 15 improvements organized by priority.

Implement the items below, using separate agents for each step. Work sequentially. Validate, review, and commit each change before proceeding to the next. Maintain state about progress in this file. Skip items marked <SKIP>.

## Critical: error handling & robustness

### 1. Add comprehensive input validation <SKIP>

**Issue**: Functions like `download_file`, `extract_archive`, `mkdir_p` lack validation for nil/empty inputs

**Fix**: Add validation at function entry points with descriptive errors

**Example**: `.local/bin/shimlink:201` - `download_file` should validate URL format before proceeding

**Impact**: Prevents cryptic FFI errors and improves debugging

### 2. Improve error recovery in atomic operations ✓ DONE

**Issue**: `create_symlink_atomic` (.local/bin/shimlink:274-295) leaves temp files on failure in some edge cases

**Fix**: Wrap in pcall with cleanup in all paths, use proper temp file naming with PID

**Impact**: Prevents abandoned temp symlinks cluttering filesystem

**Commit**: 804f27eb5e98163fe64639f6b332a202757dfce9

### 3. Add proper cleanup on partial downloads ✓ DONE

**Issue**: `download_executable` (.local/bin/shimlink:382-491) may leave corrupt versioned directories if interrupted mid-extraction

**Fix**: Use transaction pattern - extract to temp, verify completely, then atomic move

**Impact**: Prevents corrupt installations that silently fail

**Commit**: c54ee958e09ae5a0e25f77ed9fccc4f9f38e4f01

### 4. Validate checksums before any filesystem operations ✓ DONE

**Issue**: Checksum validation happens after extraction (.local/bin/shimlink:403, 442-449), wasting time and disk

**Fix**: Calculate checksum on download, validate, then extract only if valid

**Impact**: Faster failures, less disk thrashing

**Commit**: ab96ce496e50316f9a1fe5059d953e842747b52f

### 5. Handle network errors with retry logic <SKIP>

**Issue**: `download_file` (.local/bin/shimlink:201-237) fails immediately on network errors

**Fix**: Add configurable retry with exponential backoff for transient failures

**Impact**: More reliable downloads on flaky connections

## High priority: code clarity & maintainability

### 6. Eliminate code duplication in path construction ✓ DONE

**Issue**: Path patterns repeated: `.local/bin/shimlink:547-549` duplicates logic from `get_versioned_dir`

**Fix**: Create single source of truth for path construction, use consistently

**Impact**: Reduces bugs from inconsistent path logic

**Commit**: a84d8da874627cdd7d3df92998c4d775cd8a4255

### 7. Split monolithic `download_executable` function ✓ DONE

**Issue**: 109-line function (.local/bin/shimlink:382-491) mixing download, checksum, extraction, installation

**Fix**: Extract into: `download_to_temp`, `validate_checksum`, `extract_and_prepare`, `install_to_versioned_dir`

**Impact**: Each function has single responsibility, easier to test and debug

**Implementation**: Created 4 focused functions:
- `download_to_temp` (9 lines): Downloads file to temp directory
- `validate_checksum` (16 lines): Validates checksum or updates config in force mode
- `extract_and_prepare` (32 lines): Handles archive extraction and chmod
- `install_to_versioned_dir` (60 lines): Manages installation to versioned directory with strip_components

Main `download_executable` reduced to 48 lines as a clean coordinator that calls these functions in sequence with proper error handling.

**Commit**: b63a50b5f87ba8e4da2ea5e4cf742b3e9f0e16a5

### 8. Standardize error reporting ✓ DONE

**Issue**: Mixed approaches: some return `false`, some return `nil, err`, some use `error()`

**Fix**: Use consistent `ok, err` pattern throughout; reserve `error()` for unrecoverable situations

**Impact**: Predictable error handling, easier to wrap in recovery logic

**Implementation**: Standardized 11 utility functions to consistently return `nil, err` on failure:
- File operations: `read_file`, `write_file`, `is_directory`, `calculate_sha256`
- Directory operations: `list_dir_first_entry`, `get_version_files`, `rm_rf`
- Network/system operations: `download_file`, `create_symlink_atomic`, `update_config_checksum`
- Utility functions: `get_nested`

All callers updated to handle new error pattern. Reserved `error()` for programmer errors only.

**Commit**: 14d723c570eb5f20cab86e20fe60ec5a58af9b6e

### 9. Remove redundant platform config merging ✓ DONE

**Issue**: `platform.get_config` (.local/lib/lua/platform.lua:62-87) and `platform.expand` (90-125) have overlapping logic

**Fix**: Unify into single expansion pass with clear semantics

**Impact**: Removes subtle bugs from multiple passes over same data

**Implementation**: Unified platform config handling with clear semantics:
- Renamed `get_config` to `get_platform_config` (kept alias for compatibility)
- Removed platform parameter from `expand()` - now expands all platforms once
- `get_platform_config` now works on already-expanded configs and includes url from urls table
- Enhanced `get_field` to check platform-specific fields before falling back to base fields
- Updated shimlink callers to use new unified workflow

New workflow eliminates redundant merging:
1. `config = platform.expand(config)` - single pass expanding all templates for all platforms
2. `platform_config = platform.get_platform_config(config, plat)` - extract merged view for one platform
3. `value = platform.get_field(config, field, plat)` - access fields with platform awareness

Tested with all 17 shimlink configs - all pass with correct URL generation and field access.

**Commit**: 19e17db9b0e9e0e79933d556ef3b28be9f43f7a6

### 10. Consolidate file operations library ✓ DONE

**Issue**: File helpers scattered through main script: `read_file` (68), `write_file` (78), `file_exists` (57), `is_directory` (62)

**Fix**: Extract to separate `file.lua` module with comprehensive operations

**Impact**: Reusable across projects, easier to test independently

**Implementation**: Created `/pay/home/owner/.local/lib/lua/file.lua` with 11 functions:
- File operations: `read`, `write`, `exists`, `is_directory`
- Path operations: `basename`, `dirname`, `path_join`, `expand_path`
- Directory operations: `mkdir_p`, `rm_rf`, `list_dir_first_entry`

Reduced shimlink from 956 to 837 lines (119 lines). All functions maintain original behavior with consistent `nil, err` error handling pattern. Comprehensive testing confirms all operations work correctly.

**Commit**: 48b72e1e8a5dcf07bcec3e07ac6a2c68c2734fa0

## Medium priority: additional robustness

### 11. Add file locking for concurrent updates <SKIP>

**Issue**: Multiple `shimlink update` commands could corrupt shared state

**Fix**: Use lockfile pattern around versioned directory creation

**Impact**: Safe to run parallel updates for different executables

### 12. Validate archive structure before extraction <SKIP>

**Issue**: `extract_archive` (.local/bin/shimlink:239-272) blindly extracts without checking contents

**Fix**: Peek at archive contents, validate expected files present before extraction

**Impact**: Catches malformed archives early, prevents partial installations

### 13. Add progress reporting for long operations <SKIP>

**Issue**: Large downloads appear hung with no feedback

**Fix**: Add optional progress callback to `download_file`, report chunks downloaded

**Impact**: Better UX, users know system is working

### 14. Implement proper tempfile cleanup on signals

**Issue**: SIGINT/SIGTERM during download leaves garbage in temp directories

**Fix**: Register signal handler to cleanup temp files on exit

**Impact**: Cleaner filesystem, no manual cleanup needed

### 15. Add validation for version config structure

**Issue**: `version.load_file` (.local/lib/lua/version.lua:20-48) accepts any table structure

**Fix**: Add schema validation - required fields, type checking

**Impact**: Catches config errors at load time rather than during operations

## Structural improvements (bonus)

### 16. Separate concerns: CLI vs library

**Current**: Main script mixes command parsing with core logic

**Better**: Extract core shimlink operations to library, keep only CLI in main script

**Benefit**: Core logic becomes reusable, testable as library

