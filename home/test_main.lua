lu = require("luaunit")

-- cosmo may not be available in all environments
local has_cosmo, cosmo = pcall(require, "cosmo")
local unix = has_cosmo and cosmo.unix or nil

local home = require("main")

-- Helper: create a mock writer that captures output
local function mock_writer()
  local output = {}
  return {
    write = function(_, str)
      table.insert(output, str)
    end,
    get = function()
      return table.concat(output)
    end,
    clear = function()
      output = {}
    end,
  }
end

-- Helper: create temp directory
local function make_temp_dir()
  local path = "/tmp/home_test_" .. os.time() .. "_" .. math.random(10000)
  if unix then
    unix.makedirs(path)
  else
    os.execute("mkdir -p " .. path)
  end
  return path
end

-- Helper: remove directory recursively
local function remove_dir(path)
  os.execute("/usr/bin/rm -rf " .. path)
end

-- Helper: write file
local function write_file(path, content)
  local f = io.open(path, "w")
  if f then
    f:write(content)
    f:close()
    return true
  end
  return false
end

-- Helper: skip test if cosmo not available
local function skip_without_cosmo()
  if not has_cosmo then
    lu.skip("requires cosmo module")
  end
end

--------------------------------------------------------------------------------
-- Test 1: parse_manifest_line - Valid format with mode
--------------------------------------------------------------------------------
function test_parse_manifest_line_with_mode()
  local entry = home.parse_manifest_line("home/.zshrc 644")
  lu.assertNotNil(entry)
  lu.assertEquals(entry.path, "home/.zshrc")
  lu.assertEquals(entry.mode, tonumber("644", 8))
end

function test_parse_manifest_line_executable_mode()
  local entry = home.parse_manifest_line("home/.local/bin/foo 755")
  lu.assertNotNil(entry)
  lu.assertEquals(entry.path, "home/.local/bin/foo")
  lu.assertEquals(entry.mode, tonumber("755", 8))
end

--------------------------------------------------------------------------------
-- Test 2: parse_manifest_line - Old format without mode
--------------------------------------------------------------------------------
function test_parse_manifest_line_no_mode()
  local entry = home.parse_manifest_line("home/.bashrc")
  lu.assertNotNil(entry)
  lu.assertEquals(entry.path, "home/.bashrc")
  lu.assertNil(entry.mode)
end

--------------------------------------------------------------------------------
-- Test 3: parse_manifest_line - Comments and empty lines
--------------------------------------------------------------------------------
function test_parse_manifest_line_comment()
  lu.assertNil(home.parse_manifest_line("# this is a comment"))
  lu.assertNil(home.parse_manifest_line("  # indented comment"))
end

function test_parse_manifest_line_empty()
  lu.assertNil(home.parse_manifest_line(""))
  lu.assertNil(home.parse_manifest_line("   "))
  lu.assertNil(home.parse_manifest_line("\t\t"))
end

--------------------------------------------------------------------------------
-- Test 4: parse_manifest - Full manifest parsing
--------------------------------------------------------------------------------
function test_parse_manifest_string()
  local manifest = [[
# Comment
home/.zshrc 644
home/.config/nvim/init.lua 644

home/.local/bin/tool 755
]]
  local files = home.parse_manifest(manifest)
  lu.assertEquals(#files, 3)
  lu.assertEquals(files[1].path, "home/.zshrc")
  lu.assertEquals(files[2].path, "home/.config/nvim/init.lua")
  lu.assertEquals(files[3].path, "home/.local/bin/tool")
end

function test_parse_manifest_iterator()
  local lines = { "home/.zshrc 644", "# skip", "home/.bashrc" }
  local i = 0
  local iter = function()
    i = i + 1
    return lines[i]
  end
  local files = home.parse_manifest(iter)
  lu.assertEquals(#files, 2)
end

--------------------------------------------------------------------------------
-- Test 5: extract_tools - Tool extraction from paths
--------------------------------------------------------------------------------
function test_extract_tools_basic()
  local files = {
    { path = "home/.zshrc", mode = 420 },
    { path = "home/.local/bin/nvim", mode = 493 },
    { path = "home/.local/bin/ripgrep", mode = 493 },
    { path = "home/.config/foo", mode = 420 },
  }
  local tools = home.extract_tools(files)
  lu.assertEquals(#tools, 2)
  lu.assertEquals(tools[1], "nvim")
  lu.assertEquals(tools[2], "ripgrep")
end

function test_extract_tools_sorted()
  local files = {
    { path = "home/.local/bin/zsh", mode = 493 },
    { path = "home/.local/bin/abc", mode = 493 },
    { path = "home/.local/bin/make", mode = 493 },
  }
  local tools = home.extract_tools(files)
  lu.assertEquals(tools[1], "abc")
  lu.assertEquals(tools[2], "make")
  lu.assertEquals(tools[3], "zsh")
end

function test_extract_tools_no_duplicates()
  local files = {
    { path = "home/.local/bin/foo", mode = 493 },
    { path = "home/.local/bin/foo", mode = 493 },
  }
  local tools = home.extract_tools(files)
  lu.assertEquals(#tools, 1)
end

function test_extract_tools_ignores_subdirs()
  local files = {
    { path = "home/.local/bin/subdir/tool", mode = 493 },
  }
  local tools = home.extract_tools(files)
  lu.assertEquals(#tools, 0)
end

--------------------------------------------------------------------------------
-- Test 6: parse_args - Argument parsing
--------------------------------------------------------------------------------
function test_parse_args_unpack_basic()
  local args = { "unpack", "/tmp/dest" }
  local parsed = home.parse_args(args)
  lu.assertEquals(parsed.cmd, "unpack")
  lu.assertEquals(parsed.dest, "/tmp/dest")
  lu.assertFalse(parsed.force)
end

function test_parse_args_unpack_force_long()
  local args = { "unpack", "--force", "/tmp/dest" }
  local parsed = home.parse_args(args)
  lu.assertTrue(parsed.force)
  lu.assertEquals(parsed.dest, "/tmp/dest")
end

function test_parse_args_unpack_force_short()
  local args = { "unpack", "-f", "/tmp/dest" }
  local parsed = home.parse_args(args)
  lu.assertTrue(parsed.force)
  lu.assertEquals(parsed.dest, "/tmp/dest")
end

function test_parse_args_force_after_dest()
  local args = { "unpack", "/tmp/dest", "--force" }
  local parsed = home.parse_args(args)
  lu.assertTrue(parsed.force)
  lu.assertEquals(parsed.dest, "/tmp/dest")
end

function test_parse_args_default_help()
  local args = {}
  local parsed = home.parse_args(args)
  lu.assertEquals(parsed.cmd, "help")
end

function test_parse_args_list()
  local args = { "list" }
  local parsed = home.parse_args(args)
  lu.assertEquals(parsed.cmd, "list")
end

--------------------------------------------------------------------------------
-- Test 7: strip_home_prefix
--------------------------------------------------------------------------------
function test_strip_home_prefix_with_prefix()
  lu.assertEquals(home.strip_home_prefix("home/.zshrc"), ".zshrc")
  lu.assertEquals(home.strip_home_prefix("home/.config/foo"), ".config/foo")
end

function test_strip_home_prefix_without_prefix()
  lu.assertEquals(home.strip_home_prefix("other/path"), "other/path")
  lu.assertEquals(home.strip_home_prefix(".zshrc"), ".zshrc")
end

function test_strip_home_prefix_edge_cases()
  lu.assertEquals(home.strip_home_prefix("home/"), "")
  lu.assertEquals(home.strip_home_prefix("home"), "home")
end

--------------------------------------------------------------------------------
-- Test 8: is_directory_path
--------------------------------------------------------------------------------
function test_is_directory_path_true()
  lu.assertTrue(home.is_directory_path("home/.config/"))
  lu.assertTrue(home.is_directory_path("foo/bar/baz/"))
end

function test_is_directory_path_false()
  lu.assertFalse(home.is_directory_path("home/.zshrc"))
  lu.assertFalse(home.is_directory_path("foo/bar"))
end

--------------------------------------------------------------------------------
-- Test 9: copy_file - Basic copy
--------------------------------------------------------------------------------
function test_copy_file_basic()
  skip_without_cosmo()
  local tmp = make_temp_dir()
  local src = tmp .. "/source.txt"
  local dst = tmp .. "/dest.txt"

  write_file(src, "hello world")

  local ok, err = home.copy_file(src, dst)
  lu.assertTrue(ok, err)

  local f = io.open(dst, "r")
  lu.assertNotNil(f)
  local content = f:read("*a")
  f:close()
  lu.assertEquals(content, "hello world")

  remove_dir(tmp)
end

function test_copy_file_with_mode()
  skip_without_cosmo()
  local tmp = make_temp_dir()
  local src = tmp .. "/source.txt"
  local dst = tmp .. "/dest.txt"

  write_file(src, "executable")

  local ok, err = home.copy_file(src, dst, tonumber("755", 8))
  lu.assertTrue(ok, err)

  local st = unix.stat(dst)
  lu.assertNotNil(st)
  local mode_bits = st:mode() & 0x1FF
  lu.assertEquals(mode_bits, tonumber("755", 8))

  remove_dir(tmp)
end

--------------------------------------------------------------------------------
-- Test 10: copy_file - Overwrite behavior
--------------------------------------------------------------------------------
function test_copy_file_no_overwrite_fails()
  skip_without_cosmo()
  local tmp = make_temp_dir()
  local src = tmp .. "/source.txt"
  local dst = tmp .. "/dest.txt"

  write_file(src, "source content")
  write_file(dst, "existing content")

  local ok, err = home.copy_file(src, dst, nil, false)
  lu.assertFalse(ok)
  lu.assertStrContains(err, "already exists")

  local f = io.open(dst, "r")
  local content = f:read("*a")
  f:close()
  lu.assertEquals(content, "existing content")

  remove_dir(tmp)
end

function test_copy_file_overwrite_succeeds()
  skip_without_cosmo()
  local tmp = make_temp_dir()
  local src = tmp .. "/source.txt"
  local dst = tmp .. "/dest.txt"

  write_file(src, "new content")
  write_file(dst, "old content")

  local ok, err = home.copy_file(src, dst, nil, true)
  lu.assertTrue(ok, err)

  local f = io.open(dst, "r")
  local content = f:read("*a")
  f:close()
  lu.assertEquals(content, "new content")

  remove_dir(tmp)
end

--------------------------------------------------------------------------------
-- Test: copy_file - Source doesn't exist
--------------------------------------------------------------------------------
function test_copy_file_source_missing()
  skip_without_cosmo()
  local tmp = make_temp_dir()
  local ok, err = home.copy_file(tmp .. "/nonexistent", tmp .. "/dest")
  lu.assertFalse(ok)
  lu.assertStrContains(err, "failed to open source")
  remove_dir(tmp)
end

--------------------------------------------------------------------------------
-- Test: cmd_version output
--------------------------------------------------------------------------------
function test_cmd_version()
  local stdout = mock_writer()
  local code = home.cmd_version({ stdout = stdout })
  lu.assertEquals(code, 0)
  lu.assertStrContains(stdout:get(), "home built")
end

--------------------------------------------------------------------------------
-- Test: cmd_help output
--------------------------------------------------------------------------------
function test_cmd_help()
  local stderr = mock_writer()
  local code = home.cmd_help({ stderr = stderr })
  lu.assertEquals(code, 0)
  local output = stderr:get()
  lu.assertStrContains(output, "usage:")
  lu.assertStrContains(output, "unpack")
  lu.assertStrContains(output, "list")
  lu.assertStrContains(output, "--force")
end

--------------------------------------------------------------------------------
-- Test: cmd_unpack without dest returns error
--------------------------------------------------------------------------------
function test_cmd_unpack_no_dest()
  local stderr = mock_writer()
  local code = home.cmd_unpack(nil, false, { stderr = stderr })
  lu.assertEquals(code, 1)
  lu.assertStrContains(stderr:get(), "destination path required")
end

--------------------------------------------------------------------------------
-- Test: cmd_list with mock manifest
--------------------------------------------------------------------------------
function test_cmd_list()
  local tmp = make_temp_dir()
  local manifest_path = tmp .. "/MANIFEST.txt"
  write_file(manifest_path, [[
home/.zshrc 644
home/.local/bin/nvim 755
home/.local/bin/rg 755
]])

  local stdout = mock_writer()
  local stderr = mock_writer()
  local code = home.cmd_list({
    manifest_path = manifest_path,
    stdout = stdout,
    stderr = stderr,
  })
  lu.assertEquals(code, 0)

  local output = stdout:get()
  lu.assertStrContains(output, "embedded files: 3 total")
  lu.assertStrContains(output, "nvim")
  lu.assertStrContains(output, "rg")

  remove_dir(tmp)
end

--------------------------------------------------------------------------------
-- Test: main dispatch
--------------------------------------------------------------------------------
function test_main_version()
  local stdout = mock_writer()
  local code = home.main({ "version" }, { stdout = stdout })
  lu.assertEquals(code, 0)
  lu.assertStrContains(stdout:get(), "home built")
end

function test_main_help()
  local stderr = mock_writer()
  local code = home.main({ "help" }, { stderr = stderr })
  lu.assertEquals(code, 0)
  lu.assertStrContains(stderr:get(), "usage:")
end

function test_main_unknown_command()
  local stderr = mock_writer()
  local code = home.main({ "unknown" }, { stderr = stderr })
  lu.assertEquals(code, 1)
end

--------------------------------------------------------------------------------
-- Test: read_file
--------------------------------------------------------------------------------
function test_read_file_success()
  local tmp = make_temp_dir()
  local path = tmp .. "/test.txt"
  write_file(path, "test content")

  local data, err = home.read_file(path)
  lu.assertNil(err)
  lu.assertEquals(data, "test content")

  remove_dir(tmp)
end

function test_read_file_not_found()
  local data, err = home.read_file("/nonexistent/path/file.txt")
  lu.assertNil(data)
  lu.assertStrContains(err, "failed to open")
end

os.exit(lu.LuaUnit.run())
