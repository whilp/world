# lib/work/cook.mk - work module tests
# note: these tests require luaposix and will skip if not available

work_src := $(filter-out lib/work/test%.lua,$(wildcard lib/work/*.lua))

TEST_STAMPS += o/lib/work/test_backup.lua.ok
TEST_STAMPS += o/lib/work/test_blocked_on_display.lua.ok
TEST_STAMPS += o/lib/work/test_blockers.lua.ok
TEST_STAMPS += o/lib/work/test_command_blocked.lua.ok
TEST_STAMPS += o/lib/work/test_file_locking.lua.ok
TEST_STAMPS += o/lib/work/test_orphaned_blocks.lua.ok
TEST_STAMPS += o/lib/work/test_string_sanitization.lua.ok
TEST_STAMPS += o/lib/work/test_validate_blocks.lua.ok

o/lib/work/test_backup.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rwc:/tmp rw:/dev/null
o/lib/work/test_backup.lua.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/lib/work/test_backup.lua.ok: private .CPU = 30
o/lib/work/test_backup.lua.ok: $(lua_test) lib/work/test_backup.lua $(work_src)
	@mkdir -p $(@D)
	$(lua_test) lib/work/test_backup.lua
	@touch $@

o/lib/work/test_blocked_on_display.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rw:/dev/null
o/lib/work/test_blocked_on_display.lua.ok: private .PLEDGE = stdio rpath proc exec
o/lib/work/test_blocked_on_display.lua.ok: private .CPU = 30
o/lib/work/test_blocked_on_display.lua.ok: $(lua_test) lib/work/test_blocked_on_display.lua $(work_src)
	@mkdir -p $(@D)
	$(lua_test) lib/work/test_blocked_on_display.lua
	@touch $@

o/lib/work/test_blockers.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rw:/dev/null
o/lib/work/test_blockers.lua.ok: private .PLEDGE = stdio rpath proc exec
o/lib/work/test_blockers.lua.ok: private .CPU = 30
o/lib/work/test_blockers.lua.ok: $(lua_test) lib/work/test_blockers.lua $(work_src)
	@mkdir -p $(@D)
	$(lua_test) lib/work/test_blockers.lua
	@touch $@

o/lib/work/test_command_blocked.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rw:/dev/null
o/lib/work/test_command_blocked.lua.ok: private .PLEDGE = stdio rpath proc exec
o/lib/work/test_command_blocked.lua.ok: private .CPU = 30
o/lib/work/test_command_blocked.lua.ok: $(lua_test) lib/work/test_command_blocked.lua $(work_src)
	@mkdir -p $(@D)
	$(lua_test) lib/work/test_command_blocked.lua
	@touch $@

o/lib/work/test_file_locking.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rwc:/tmp rw:/dev/null
o/lib/work/test_file_locking.lua.ok: private .PLEDGE = stdio rpath wpath cpath proc exec
o/lib/work/test_file_locking.lua.ok: private .CPU = 30
o/lib/work/test_file_locking.lua.ok: $(lua_test) lib/work/test_file_locking.lua $(work_src)
	@mkdir -p $(@D)
	$(lua_test) lib/work/test_file_locking.lua
	@touch $@

o/lib/work/test_orphaned_blocks.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rw:/dev/null
o/lib/work/test_orphaned_blocks.lua.ok: private .PLEDGE = stdio rpath proc exec
o/lib/work/test_orphaned_blocks.lua.ok: private .CPU = 30
o/lib/work/test_orphaned_blocks.lua.ok: $(lua_test) lib/work/test_orphaned_blocks.lua $(work_src)
	@mkdir -p $(@D)
	$(lua_test) lib/work/test_orphaned_blocks.lua
	@touch $@

o/lib/work/test_string_sanitization.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rw:/dev/null
o/lib/work/test_string_sanitization.lua.ok: private .PLEDGE = stdio rpath proc exec
o/lib/work/test_string_sanitization.lua.ok: private .CPU = 30
o/lib/work/test_string_sanitization.lua.ok: $(lua_test) lib/work/test_string_sanitization.lua $(work_src)
	@mkdir -p $(@D)
	$(lua_test) lib/work/test_string_sanitization.lua
	@touch $@

o/lib/work/test_validate_blocks.lua.ok: private .UNVEIL = r:lib rx:$(lua_test) rw:/dev/null
o/lib/work/test_validate_blocks.lua.ok: private .PLEDGE = stdio rpath proc exec
o/lib/work/test_validate_blocks.lua.ok: private .CPU = 30
o/lib/work/test_validate_blocks.lua.ok: $(lua_test) lib/work/test_validate_blocks.lua $(work_src)
	@mkdir -p $(@D)
	$(lua_test) lib/work/test_validate_blocks.lua
	@touch $@
