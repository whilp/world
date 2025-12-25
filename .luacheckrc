std = "lua54"
max_line_length = 120
allow_defined_top = true

ignore = {
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

-- Test file suppressions (211: unused variable is acceptable in tests)
files["lib/claude/test.lua"] = { ignore = { "211" } }
files["lib/home/test_main.lua"] = { ignore = { "211" } }
files["lib/nvim/test.lua"] = { ignore = { "211" } }
files["lib/test_daemonize.lua"] = { ignore = { "211" } }
files["lib/test_whereami.lua"] = { ignore = { "211" } }
files["lib/work/test_backup.lua"] = { ignore = { "211", "411" } }
files["lib/work/test_command_blocked.lua"] = { ignore = { "211" } }
files["lib/work/test_file_locking.lua"] = { ignore = { "211", "411" } }
files["lib/work/test_orphaned_blocks.lua"] = { ignore = { "211" } }
files["lib/work/test_validate_blocks.lua"] = { ignore = { "211" } }

-- lib/run-test.lua uses setfenv which triggers 122 (setting read-only global)
files["lib/run-test.lua"] = { ignore = { "122" } }
