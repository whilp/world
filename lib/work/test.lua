#!/usr/bin/env lua

local lu = require("luaunit")

require("work.test_command_blocked")
require("work.test_blockers")
require("work.test_blocked_on_display")
require("work.test_validate_blocks")
require("work.test_orphaned_blocks")
require("work.test_file_locking")
require("work.test_string_sanitization")
require("work.test_backup")

os.exit(lu.LuaUnit.run())
