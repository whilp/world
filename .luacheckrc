std = "lua54"
max_line_length = 120

include_files = {
  "lib",
  "3p",
}

exclude_files = {
  "**/*.mk",
  "**/*.ok",
  "lib/home/.args",
  "lib/home/MANIFEST.txt",
}

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
  "111/Test.*",   -- setting test class globals
  "111/test_.*",  -- setting test function globals
  "112/Test.*",   -- mutating test class globals
  "212/self",     -- unused self in test methods
}

files = {
  ["lib/build/test.lua"] = {
    globals = { "arg" },
  },
  ["lib/claude/version.lua"] = {
    -- Generated file with long URLs
    max_line_length = false,
  },
}
