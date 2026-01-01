local lu = require("luaunit")
local unix = require("cosmo.unix")
local cosmo = require("cosmo")
local path = require("cosmo.path")
local spawn = require("spawn")

-- use cosmos lua instead of system commands to avoid /usr unveil
local lua_bin = path.join(os.getenv("TEST_BIN_DIR"), "bin", "lua")

TestSpawn = {}

function TestSpawn:test_simple_command()
	local handle = spawn({lua_bin, "-e", "print('hello')"})
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
	local handle = spawn({lua_bin, "-e", "os.exit(0)"})
	lu.assertNotNil(handle)
	local exit_code = handle:wait()
	lu.assertEquals(exit_code, 0)

	handle = spawn({lua_bin, "-e", "os.exit(1)"})
	lu.assertNotNil(handle)
	exit_code = handle:wait()
	lu.assertEquals(exit_code, 1)
end

function TestSpawn:test_wait_drains_stdout_to_avoid_sigpipe()
	local tmpdir = path.join(TEST_TMPDIR, "spawn_test")
	unix.makedirs(tmpdir)
	local tmp_file = path.join(tmpdir, "checkfile")
	local tmp_target = path.join(tmpdir, "target")

	local f = io.open(tmp_target, "w")
	f:write("test content\n")
	f:close()

	-- compute sha256 using cosmo (returns hex)
	local content = "test content\n"
	local expected_sha = cosmo.EncodeHex(cosmo.Sha256(content))

	f = io.open(tmp_file, "w")
	f:write(expected_sha .. "  " .. tmp_target .. "\n")
	f:close()

	-- verify using lua script that reads file and computes hash
	local script = string.format([[
		local cosmo = require("cosmo")
		local f = io.open(%q, "r")
		local content = f:read("*a")
		f:close()
		local hash = cosmo.EncodeHex(cosmo.Sha256(content))
		f = io.open(%q, "r")
		local expected = f:read("*l"):match("^(%%x+)")
		f:close()
		if hash == expected then os.exit(0) else os.exit(1) end
	]], tmp_target, tmp_file)

	local handle = spawn({lua_bin, "-e", script})
	lu.assertNotNil(handle)

	local exit_code, err = handle:wait()
	lu.assertEquals(exit_code, 0, "sha check should exit 0, got: " .. tostring(err))

	unix.unlink(tmp_file)
	unix.unlink(tmp_target)
	unix.rmdir(tmpdir)
end

function TestSpawn:test_stdin_string()
	local handle = spawn({lua_bin, "-e", "io.write(io.read('*a'))"}, {stdin = "hello from stdin"})
	lu.assertNotNil(handle)
	local ok, out = handle:read()
	lu.assertTrue(ok)
	lu.assertEquals(out, "hello from stdin")
end

function TestSpawn:test_read_captures_output()
	local handle = spawn({lua_bin, "-e", [[io.write("line1\nline2\n")]]})
	lu.assertNotNil(handle)
	local ok, out = handle:read()
	lu.assertTrue(ok)
	lu.assertEquals(out, "line1\nline2\n")
end

