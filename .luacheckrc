std = "lua54"
max_line_length = 120

-- Fennel-inspired strictness
--
-- Fennel addresses several Lua language issues through stricter defaults:
--   1. Accidental globals: Fennel makes globals difficult to use unintentionally
--      to prevent typos and scoping errors
--   2. Unrestricted reassignment: Fennel distinguishes immutable 'let' from
--      reassignable 'var', promoting cleaner code patterns
--   3. Missing arity checks: Fennel's 'lambda' enforces argument checking
--
-- This configuration enforces similar discipline:
--
-- Global variable checks (enabled by default):
--   111: Setting an undefined global variable
--   112: Mutating an undefined global variable
--   113: Accessing an undefined global variable
--   131: Unused implicitly defined global variable (except test_* patterns)
--
-- Variable shadowing and redefinition (Fennel's immutability-by-default):
--   411: Redefining a local variable
--   421: Shadowing a local variable
--   422: Shadowing an argument
--   423: Shadowing a loop variable
--
-- Unused values (helps catch typos and logic errors):
--   311: Value assigned to a local variable is unused
--   312: Value of an argument is unused
--   313: Value of a loop variable is unused
--
-- See: https://github.com/bakpakin/Fennel/blob/main/rationale.md

ignore = {
  "111/test_.*",
  "111/Test.*",
  "112/Test.*",
  "113/Test.*",
  "131/test_.*",
  "131/Test.*",
  "212/self",
}

read_globals = {
  "jit",
  "setfenv",
}

globals = {
  "lu",
}

-- Test file suppressions
-- 211: unused variable is acceptable in tests
-- 411: redefining locals is common in test setup/teardown
-- 421-423: shadowing is acceptable in test contexts
files["lib/claude/test.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/environ/test.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/home/test_main.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/nvim/test.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/spawn/test_spawn.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/test_daemonize.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/test_whereami.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/work/test.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/work/test_backup.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/work/test_blocked_on_display.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/work/test_blockers.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/work/test_command_blocked.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/work/test_file_locking.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/work/test_orphaned_blocks.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/work/test_string_sanitization.lua"] = { ignore = { "211", "411", "421", "422", "423" } }
files["lib/work/test_validate_blocks.lua"] = { ignore = { "211", "411", "421", "422", "423" } }

-- lib/run-test.lua uses setfenv which triggers 122 (setting read-only global)
files["lib/run-test.lua"] = { ignore = { "122" } }
