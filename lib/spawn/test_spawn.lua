local lu = require("luaunit")
local unix = require("cosmo.unix")
local spawn = require("spawn").spawn

TestSpawn = {}

function TestSpawn:test_simple_command()
	local handle = spawn({"echo", "hello"})
	lu.assertNotNil(handle)
	local ok, out = handle:read()
	lu.assertTrue(ok)
	lu.assertEquals(out, "hello\n")
end

function TestSpawn:test_command_not_found()
	local handle, err = spawn({"nonexistent_command_12345"})
	lu.assertNil(handle)
	lu.assertStrContains(err, "command not found")
end

function TestSpawn:test_wait_returns_exit_code()
	local handle = spawn({"true"})
	lu.assertNotNil(handle)
	local exit_code = handle:wait()
	lu.assertEquals(exit_code, 0)

	handle = spawn({"false"})
	lu.assertNotNil(handle)
	exit_code = handle:wait()
	lu.assertEquals(exit_code, 1)
end

function TestSpawn:test_wait_drains_stdout_to_avoid_sigpipe()
	local tmp_file = "/tmp/spawn_test_checkfile"
	local tmp_target = "/tmp/spawn_test_target"

	local f = io.open(tmp_target, "w")
	f:write("test content\n")
	f:close()

	local ok, sha_out = spawn({"shasum", "-a", "256", tmp_target}):read()
	lu.assertTrue(ok)
	local expected_sha = sha_out:match("^(%x+)")

	f = io.open(tmp_file, "w")
	f:write(expected_sha .. "  " .. tmp_target .. "\n")
	f:close()

	local handle = spawn({"shasum", "-a", "256", "-c", tmp_file})
	lu.assertNotNil(handle)

	local exit_code, err = handle:wait()
	lu.assertEquals(exit_code, 0, "shasum should exit 0, got: " .. tostring(err))

	unix.unlink(tmp_file)
	unix.unlink(tmp_target)
end

function TestSpawn:test_stdin_string()
	local handle = spawn({"cat"}, {stdin = "hello from stdin"})
	lu.assertNotNil(handle)
	local ok, out = handle:read()
	lu.assertTrue(ok)
	lu.assertEquals(out, "hello from stdin")
end

function TestSpawn:test_read_captures_output()
	local handle = spawn({"printf", "line1\\nline2\\n"})
	lu.assertNotNil(handle)
	local ok, out = handle:read()
	lu.assertTrue(ok)
	lu.assertEquals(out, "line1\nline2\n")
end

os.exit(lu.LuaUnit.run())
