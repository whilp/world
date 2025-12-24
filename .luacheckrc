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

-- Baseline suppressions - tighten over time
-- 211: unused variable
-- 212: unused argument
-- 213: unused loop variable
-- 311: value assigned is unused
-- 411: variable was previously defined
-- 412: variable previously defined on line N
-- 421: shadowing definition of variable
-- 431: shadowing upvalue

files[".config/setup/test.lua"] = { ignore = { "211" } }
files[".config/setup/util.lua"] = { ignore = { "211" } }

files["lib/claude/test.lua"] = { ignore = { "211" } }

files["lib/home/main.lua"] = { ignore = { "211", "431" } }
files["lib/home/test_main.lua"] = { ignore = { "211", "431" } }

files["lib/nvim/test.lua"] = { ignore = { "211" } }

files["lib/run-test.lua"] = { ignore = { "122" } }

files["lib/symbols/dump.lua"] = { ignore = { "411" } }

files["lib/test_daemonize.lua"] = { ignore = { "211" } }
files["lib/test_whereami.lua"] = { ignore = { "211" } }

files["lib/work/api.lua"] = { ignore = { "411", "421" } }
files["lib/work/data.lua"] = { ignore = { "211", "411", "421", "431" } }
files["lib/work/process.lua"] = { ignore = { "211" } }
files["lib/work/render.lua"] = { ignore = { "212", "311" } }
files["lib/work/validate.lua"] = { ignore = { "212" } }

files["lib/work/test_backup.lua"] = { ignore = { "411" } }
files["lib/work/test_command_blocked.lua"] = { ignore = { "211" } }
files["lib/work/test_file_locking.lua"] = { ignore = { "211", "411" } }
files["lib/work/test_orphaned_blocks.lua"] = { ignore = { "211" } }
files["lib/work/test_validate_blocks.lua"] = { ignore = { "211" } }
