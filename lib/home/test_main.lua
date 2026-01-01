local lu = require("luaunit")
local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local home = require("home.main")

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
  local temp_path = "/tmp/home_test_" .. os.time() .. "_" .. math.random(10000)
  unix.makedirs(temp_path)
  return temp_path
end

--------------------------------------------------------------------------------
-- Test: parse_args - Argument parsing
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

function test_parse_args_verbose_long()
  local args = { "unpack", "--verbose", "/tmp/dest" }
  local parsed = home.parse_args(args)
  lu.assertTrue(parsed.verbose)
  lu.assertEquals(parsed.dest, "/tmp/dest")
end

function test_parse_args_verbose_short()
  local args = { "unpack", "-v", "/tmp/dest" }
  local parsed = home.parse_args(args)
  lu.assertTrue(parsed.verbose)
end

function test_parse_args_dry_run_long()
  local args = { "unpack", "--dry-run", "/tmp/dest" }
  local parsed = home.parse_args(args)
  lu.assertTrue(parsed.dry_run)
  lu.assertEquals(parsed.dest, "/tmp/dest")
end

function test_parse_args_dry_run_short()
  local args = { "unpack", "-n", "/tmp/dest" }
  local parsed = home.parse_args(args)
  lu.assertTrue(parsed.dry_run)
end

function test_parse_args_only()
  local args = { "unpack", "--only", "/tmp/dest" }
  local parsed = home.parse_args(args)
  lu.assertTrue(parsed.only)
  lu.assertEquals(parsed.dest, "/tmp/dest")
end

function test_parse_args_with_platform()
  local args = { "unpack", "--with-platform", "/tmp/dest" }
  local parsed = home.parse_args(args)
  lu.assertTrue(parsed.with_platform)
  lu.assertEquals(parsed.dest, "/tmp/dest")
end

function test_parse_args_combined_flags()
  local args = { "unpack", "--force", "--verbose", "--dry-run", "/tmp/dest" }
  local parsed = home.parse_args(args)
  lu.assertTrue(parsed.force)
  lu.assertTrue(parsed.verbose)
  lu.assertTrue(parsed.dry_run)
  lu.assertEquals(parsed.dest, "/tmp/dest")
end

--------------------------------------------------------------------------------
-- Test: copy_file - Basic copy
--------------------------------------------------------------------------------
function test_copy_file_basic()
  local tmp = make_temp_dir()
  local src = path.join(tmp, "source.txt")
  local dst = path.join(tmp, "dest.txt")

  cosmo.Barf(src, "hello world")

  local ok, err = home.copy_file(src, dst)
  lu.assertTrue(ok, err)

  local f = io.open(dst, "r")
  lu.assertNotNil(f)
  local content = f:read("*a")
  f:close()
  lu.assertEquals(content, "hello world")

  unix.rmrf(tmp)
end

function test_copy_file_with_mode()
  local tmp = make_temp_dir()
  local src = path.join(tmp, "source.txt")
  local dst = path.join(tmp, "dest.txt")

  cosmo.Barf(src, "executable")

  local ok, err = home.copy_file(src, dst, tonumber("755", 8))
  lu.assertTrue(ok, err)

  local st = unix.stat(dst)
  lu.assertNotNil(st)
  local mode_bits = st:mode() & 0x1FF
  lu.assertEquals(mode_bits, tonumber("755", 8))

  unix.rmrf(tmp)
end

--------------------------------------------------------------------------------
-- Test: copy_file - Overwrite behavior
--------------------------------------------------------------------------------
function test_copy_file_no_overwrite_fails()
  local tmp = make_temp_dir()
  local src = path.join(tmp, "source.txt")
  local dst = path.join(tmp, "dest.txt")

  cosmo.Barf(src, "source content")
  cosmo.Barf(dst, "existing content")

  local ok, err = home.copy_file(src, dst, nil, false)
  lu.assertFalse(ok)
  lu.assertStrContains(err, "already exists")

  local f = io.open(dst, "r")
  local content = f:read("*a")
  f:close()
  lu.assertEquals(content, "existing content")

  unix.rmrf(tmp)
end

function test_copy_file_overwrite_succeeds()
  local tmp = make_temp_dir()
  local src = path.join(tmp, "source.txt")
  local dst = path.join(tmp, "dest.txt")

  cosmo.Barf(src, "new content")
  cosmo.Barf(dst, "old content")

  local ok, err = home.copy_file(src, dst, nil, true)
  lu.assertTrue(ok, err)

  local f = io.open(dst, "r")
  local content = f:read("*a")
  f:close()
  lu.assertEquals(content, "new content")

  unix.rmrf(tmp)
end

--------------------------------------------------------------------------------
-- Test: copy_file - Source doesn't exist
--------------------------------------------------------------------------------
function test_copy_file_source_missing()
  local tmp = make_temp_dir()
  local ok, err = home.copy_file(path.join(tmp, "nonexistent"), path.join(tmp, "dest"))
  lu.assertFalse(ok)
  lu.assertStrContains(err, "failed to open source")
  unix.rmrf(tmp)
end

--------------------------------------------------------------------------------
-- Test: cmd_version output
--------------------------------------------------------------------------------
function test_cmd_version()
  local stdout = mock_writer()
  local code = home.cmd_version({ stdout = stdout })
  lu.assertEquals(code, 0)
  lu.assertStrContains(stdout:get(), "home")
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
-- Test: cmd_unpack silent by default
--------------------------------------------------------------------------------
function test_cmd_unpack_silent_by_default()
  local tmp = make_temp_dir()
  local zip_root = path.join(tmp, "zip/")
  unix.makedirs(zip_root)

  cosmo.Barf(zip_root .. ".testfile", "test content")

  local manifest = {
    files = {
      [".testfile"] = { mode = 420 },
    },
  }

  local dest = path.join(tmp, "dest")
  local stderr = mock_writer()
  local stdout = mock_writer()

  local code = home.cmd_unpack(dest, false, {
    manifest = manifest,
    zip_root = zip_root,
    stderr = stderr,
    stdout = stdout,
    verbose = false,
  })

  lu.assertEquals(code, 0)
  lu.assertEquals(stderr:get(), "")
  lu.assertEquals(stdout:get(), "")

  unix.rmrf(tmp)
end

--------------------------------------------------------------------------------
-- Test: cmd_unpack verbose mode
--------------------------------------------------------------------------------
function test_cmd_unpack_verbose()
  local tmp = make_temp_dir()
  local zip_root = path.join(tmp, "zip/")
  unix.makedirs(zip_root)

  cosmo.Barf(zip_root .. ".zshrc", "zsh content")
  cosmo.Barf(zip_root .. ".bashrc", "bash content")

  local manifest = {
    files = {
      [".zshrc"] = { mode = 420 },
      [".bashrc"] = { mode = 420 },
    },
  }

  local dest = path.join(tmp, "dest")
  local stdout = mock_writer()

  local code = home.cmd_unpack(dest, false, {
    manifest = manifest,
    zip_root = zip_root,
    stdout = stdout,
    verbose = true,
  })

  lu.assertEquals(code, 0)
  local output = stdout:get()
  lu.assertStrContains(output, ".zshrc\n")
  lu.assertStrContains(output, ".bashrc\n")

  unix.rmrf(tmp)
end

function test_cmd_unpack_verbose_force_overwrite()
  local tmp = make_temp_dir()
  local zip_root = path.join(tmp, "zip/")
  unix.makedirs(zip_root)

  cosmo.Barf(zip_root .. ".testfile", "new content")

  local manifest = {
    files = {
      [".testfile"] = { mode = 420 },
    },
  }

  local dest = path.join(tmp, "dest")
  unix.makedirs(dest)
  cosmo.Barf(path.join(dest, ".testfile"), "old content")

  local stdout = mock_writer()

  local code = home.cmd_unpack(dest, true, {
    manifest = manifest,
    zip_root = zip_root,
    stdout = stdout,
    verbose = true,
  })

  lu.assertEquals(code, 0)
  local output = stdout:get()
  lu.assertStrContains(output, ".testfile (overwritten)\n")

  unix.rmrf(tmp)
end

--------------------------------------------------------------------------------
-- Test: cmd_unpack dry-run mode
--------------------------------------------------------------------------------
function test_cmd_unpack_dry_run()
  local tmp = make_temp_dir()
  local zip_root = path.join(tmp, "zip/")
  unix.makedirs(zip_root)

  cosmo.Barf(zip_root .. ".testfile", "test content")

  local manifest = {
    files = {
      [".testfile"] = { mode = 420 },
    },
  }

  local dest = path.join(tmp, "dest")

  local code = home.cmd_unpack(dest, false, {
    manifest = manifest,
    zip_root = zip_root,
    dry_run = true,
  })

  lu.assertEquals(code, 0)

  -- Verify file was NOT actually created
  local f = io.open(path.join(dest, ".testfile"), "r")
  lu.assertNil(f)

  unix.rmrf(tmp)
end

function test_cmd_unpack_dry_run_verbose()
  local tmp = make_temp_dir()
  local zip_root = path.join(tmp, "zip/")
  unix.makedirs(zip_root)

  cosmo.Barf(zip_root .. ".zshrc", "zsh content")

  local manifest = {
    files = {
      [".zshrc"] = { mode = 420 },
    },
  }

  local dest = path.join(tmp, "dest")
  local stdout = mock_writer()

  local code = home.cmd_unpack(dest, false, {
    manifest = manifest,
    zip_root = zip_root,
    stdout = stdout,
    dry_run = true,
    verbose = true,
  })

  lu.assertEquals(code, 0)
  local output = stdout:get()
  lu.assertStrContains(output, ".zshrc\n")

  -- Verify file was NOT actually created
  local f = io.open(path.join(dest, ".zshrc"), "r")
  lu.assertNil(f)

  unix.rmrf(tmp)
end

--------------------------------------------------------------------------------
-- Test: cmd_unpack --only filter
--------------------------------------------------------------------------------
function test_cmd_unpack_only_filter()
  local tmp = make_temp_dir()
  local zip_root = path.join(tmp, "zip/")
  unix.makedirs(zip_root)

  cosmo.Barf(zip_root .. ".zshrc", "zsh content")
  cosmo.Barf(zip_root .. ".bashrc", "bash content")
  cosmo.Barf(zip_root .. ".vimrc", "vim content")

  local manifest = {
    files = {
      [".zshrc"] = { mode = 420 },
      [".bashrc"] = { mode = 420 },
      [".vimrc"] = { mode = 420 },
    },
  }

  local dest = path.join(tmp, "dest")

  -- Simulate stdin input with only .zshrc and .vimrc
  local filter_input = ".zshrc\n.vimrc\n"

  local code = home.cmd_unpack(dest, false, {
    manifest = manifest,
    zip_root = zip_root,
    only = true,
    filter_input = filter_input,
  })

  lu.assertEquals(code, 0)

  -- Verify only filtered files were created
  local zsh = io.open(path.join(dest, ".zshrc"), "r")
  lu.assertNotNil(zsh)
  zsh:close()

  local vim = io.open(path.join(dest, ".vimrc"), "r")
  lu.assertNotNil(vim)
  vim:close()

  local bash = io.open(path.join(dest, ".bashrc"), "r")
  lu.assertNil(bash)

  unix.rmrf(tmp)
end

function test_cmd_unpack_only_empty_filter()
  local tmp = make_temp_dir()
  local zip_root = path.join(tmp, "zip/")
  unix.makedirs(zip_root)

  cosmo.Barf(zip_root .. ".zshrc", "zsh content")

  local manifest = {
    files = {
      [".zshrc"] = { mode = 420 },
    },
  }

  local dest = path.join(tmp, "dest")

  -- Empty filter = extract nothing
  local code = home.cmd_unpack(dest, false, {
    manifest = manifest,
    zip_root = zip_root,
    only = true,
    filter_input = "",
  })

  lu.assertEquals(code, 0)

  local f = io.open(path.join(dest, ".zshrc"), "r")
  lu.assertNil(f)

  unix.rmrf(tmp)
end

function test_cmd_unpack_only_null_delimited()
  local tmp = make_temp_dir()
  local zip_root = path.join(tmp, "zip/")
  unix.makedirs(zip_root)

  cosmo.Barf(zip_root .. ".zshrc", "zsh content")
  cosmo.Barf(zip_root .. ".bashrc", "bash content")
  cosmo.Barf(zip_root .. ".vimrc", "vim content")

  local manifest = {
    files = {
      [".zshrc"] = { mode = 420 },
      [".bashrc"] = { mode = 420 },
      [".vimrc"] = { mode = 420 },
    },
  }

  local dest = path.join(tmp, "dest")

  -- Null-delimited input
  local filter_input = ".zshrc" .. string.char(0) .. ".vimrc" .. string.char(0)

  local code = home.cmd_unpack(dest, false, {
    manifest = manifest,
    zip_root = zip_root,
    only = true,
    null = true,
    filter_input = filter_input,
  })

  lu.assertEquals(code, 0)

  -- Verify only filtered files were created
  local zsh = io.open(path.join(dest, ".zshrc"), "r")
  lu.assertNotNil(zsh)
  zsh:close()

  local vim = io.open(path.join(dest, ".vimrc"), "r")
  lu.assertNotNil(vim)
  vim:close()

  local bash = io.open(path.join(dest, ".bashrc"), "r")
  lu.assertNil(bash)

  unix.rmrf(tmp)
end

--------------------------------------------------------------------------------
-- Test: cmd_list
--------------------------------------------------------------------------------
function test_cmd_list_default_paths_only()
  local manifest = {
    files = {
      [".zshrc"] = { mode = 420 },
      [".local/bin/nvim"] = { mode = 493 },
    },
  }

  local stdout = mock_writer()
  local stderr = mock_writer()
  local code = home.cmd_list({
    manifest = manifest,
    stdout = stdout,
    stderr = stderr,
  })
  lu.assertEquals(code, 0)

  local output = stdout:get()
  lu.assertStrContains(output, ".zshrc\n")
  lu.assertStrContains(output, ".local/bin/nvim\n")
end

function test_cmd_list_verbose()
  local manifest = {
    files = {
      [".zshrc"] = { mode = 420 },
      [".local/bin/nvim"] = { mode = 493 },
    },
  }

  local stdout = mock_writer()
  local stderr = mock_writer()
  local code = home.cmd_list({
    manifest = manifest,
    stdout = stdout,
    stderr = stderr,
    verbose = true,
  })
  lu.assertEquals(code, 0)

  local output = stdout:get()
  lu.assertStrContains(output, "-rw-r--r-- .zshrc\n")
  lu.assertStrContains(output, "-rwxr-xr-x .local/bin/nvim\n")
end

function test_cmd_list_null_delimiter()
  local manifest = {
    files = {
      [".zshrc"] = { mode = 420 },
      [".bashrc"] = { mode = 420 },
    },
  }

  local stdout = mock_writer()
  local stderr = mock_writer()
  local code = home.cmd_list({
    manifest = manifest,
    stdout = stdout,
    stderr = stderr,
    null = true,
  })
  lu.assertEquals(code, 0)

  local output = stdout:get()
  lu.assertStrContains(output, ".zshrc" .. string.char(0))
  lu.assertStrContains(output, ".bashrc" .. string.char(0))
end

--------------------------------------------------------------------------------
-- Test: main dispatch
--------------------------------------------------------------------------------
function test_main_version()
  local stdout = mock_writer()
  local code = home.main({ "version" }, { stdout = stdout })
  lu.assertEquals(code, 0)
  lu.assertStrContains(stdout:get(), "home")
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
  local file_path = path.join(tmp, "test.txt")
  cosmo.Barf(file_path, "test content")

  local data, err = home.read_file(file_path)
  lu.assertNil(err)
  lu.assertEquals(data, "test content")

  unix.rmrf(tmp)
end

function test_read_file_not_found()
  local data, err = home.read_file("/nonexistent/path/file.txt")
  lu.assertNil(data)
  lu.assertStrContains(err, "failed to open")
end

--------------------------------------------------------------------------------
-- Test: format_mode - Permission string formatting
--------------------------------------------------------------------------------
function test_format_mode_regular_file_644()
  local result = home.format_mode(tonumber("644", 8), false)
  lu.assertEquals(result, "-rw-r--r--")
end

function test_format_mode_executable_755()
  local result = home.format_mode(tonumber("755", 8), false)
  lu.assertEquals(result, "-rwxr-xr-x")
end

function test_format_mode_directory()
  local result = home.format_mode(tonumber("755", 8), true)
  lu.assertEquals(result, "drwxr-xr-x")
end

function test_format_mode_no_mode()
  local result = home.format_mode(nil, false)
  lu.assertEquals(result, "----------")
end

--------------------------------------------------------------------------------
-- Test: parse_args - 3p command
--------------------------------------------------------------------------------
function test_parse_args_3p_basic()
  local args = { "3p" }
  local parsed = home.parse_args(args)
  lu.assertEquals(parsed.cmd, "3p")
  lu.assertNil(parsed.subcmd)
end

function test_parse_args_3p_list()
  local args = { "3p", "list" }
  local parsed = home.parse_args(args)
  lu.assertEquals(parsed.cmd, "3p")
  lu.assertEquals(parsed.subcmd, "list")
end

function test_parse_args_3p_verbose()
  local args = { "3p", "--verbose" }
  local parsed = home.parse_args(args)
  lu.assertEquals(parsed.cmd, "3p")
  lu.assertTrue(parsed.verbose)
  lu.assertNil(parsed.subcmd)
end

function test_parse_args_3p_dry_run()
  local args = { "3p", "--dry-run" }
  local parsed = home.parse_args(args)
  lu.assertEquals(parsed.cmd, "3p")
  lu.assertTrue(parsed.dry_run)
end

function test_parse_args_3p_list_verbose()
  local args = { "3p", "list", "-v" }
  local parsed = home.parse_args(args)
  lu.assertEquals(parsed.cmd, "3p")
  lu.assertEquals(parsed.subcmd, "list")
  lu.assertTrue(parsed.verbose)
end

--------------------------------------------------------------------------------
-- Test: find_binary_in_dir
--------------------------------------------------------------------------------
function test_find_binary_in_dir_direct()
  local tmp = make_temp_dir()
  local bin_dir = path.join(tmp, "bin")
  unix.makedirs(bin_dir)
  local bin_path = path.join(bin_dir, "mytool")
  cosmo.Barf(bin_path, "#!/bin/sh\necho test")

  local result = home.find_binary_in_dir(tmp, "mytool")
  lu.assertEquals(result, bin_path)

  unix.rmrf(tmp)
end

function test_find_binary_in_dir_in_bin()
  local tmp = make_temp_dir()
  local bin_dir = path.join(tmp, "bin")
  unix.makedirs(bin_dir)
  local bin_path = path.join(bin_dir, "mytool")
  cosmo.Barf(bin_path, "#!/bin/sh\necho test")

  local result = home.find_binary_in_dir(tmp, "mytool")
  lu.assertEquals(result, bin_path)

  unix.rmrf(tmp)
end

function test_find_binary_in_dir_not_found()
  local tmp = make_temp_dir()

  local result = home.find_binary_in_dir(tmp, "nonexistent")
  lu.assertNil(result)

  unix.rmrf(tmp)
end

--------------------------------------------------------------------------------
-- Test: scan_for_latest_version
--------------------------------------------------------------------------------
function test_scan_for_latest_version_single()
  local tmp = make_temp_dir()
  local share_dir = tmp
  local tool_dir = path.join(share_dir, "rg", "14.1.1-abcd1234")
  local tool_bin_dir = path.join(tool_dir, "bin")
  unix.makedirs(tool_bin_dir)
  local bin_path = path.join(tool_bin_dir, "rg")
  cosmo.Barf(bin_path, "#!/bin/sh\necho rg")

  local result = home.scan_for_latest_version("rg", share_dir)
  lu.assertNotNil(result)
  lu.assertEquals(result.version, "14.1.1")
  lu.assertEquals(result.sha, "abcd1234")
  lu.assertEquals(result.path, bin_path)

  unix.rmrf(tmp)
end

function test_scan_for_latest_version_multiple()
  local tmp = make_temp_dir()
  local share_dir = tmp

  local old_dir = path.join(share_dir, "rg", "14.0.0-aaa00000", "bin")
  unix.makedirs(old_dir)
  cosmo.Barf(path.join(old_dir, "rg"), "old")

  local new_dir = path.join(share_dir, "rg", "14.1.1-bbb11111", "bin")
  unix.makedirs(new_dir)
  local new_bin = path.join(new_dir, "rg")
  cosmo.Barf(new_bin, "new")

  local result = home.scan_for_latest_version("rg", share_dir)
  lu.assertNotNil(result)
  lu.assertEquals(result.version, "14.1.1")
  lu.assertEquals(result.sha, "bbb11111")
  lu.assertEquals(result.path, new_bin)

  unix.rmrf(tmp)
end

function test_scan_for_latest_version_not_found()
  local tmp = make_temp_dir()

  local result = home.scan_for_latest_version("nonexistent", tmp)
  lu.assertNil(result)

  unix.rmrf(tmp)
end

function test_scan_for_latest_version_bin_subdir()
  local tmp = make_temp_dir()
  local share_dir = tmp
  local tool_dir = path.join(share_dir, "nvim", "2025.12.07-abcd1234")
  local bin_dir = path.join(tool_dir, "bin")
  unix.makedirs(bin_dir)
  local bin_path = path.join(bin_dir, "nvim")
  cosmo.Barf(bin_path, "#!/bin/sh\necho nvim")

  local result = home.scan_for_latest_version("nvim", share_dir)
  lu.assertNotNil(result)
  lu.assertEquals(result.version, "2025.12.07")
  lu.assertEquals(result.path, bin_path)

  unix.rmrf(tmp)
end

--------------------------------------------------------------------------------
-- Test: update_symlink
--------------------------------------------------------------------------------
function test_update_symlink_create()
  local tmp = make_temp_dir()
  local target = path.join(tmp, "target")
  cosmo.Barf(target, "target content")
  local link = path.join(tmp, "link")

  local ok = home.update_symlink(link, target, {})
  lu.assertTrue(ok)

  local st = unix.stat(link, unix.AT_SYMLINK_NOFOLLOW)
  lu.assertNotNil(st)
  lu.assertTrue(unix.S_ISLNK(st:mode()))

  unix.rmrf(tmp)
end

function test_update_symlink_replace()
  local tmp = make_temp_dir()
  local old_target = path.join(tmp, "old_target")
  cosmo.Barf(old_target, "old content")
  local new_target = path.join(tmp, "new_target")
  cosmo.Barf(new_target, "new content")
  local link = path.join(tmp, "link")

  unix.symlink(old_target, link)

  local ok = home.update_symlink(link, new_target, {})
  lu.assertTrue(ok)

  local st = unix.stat(link, unix.AT_SYMLINK_NOFOLLOW)
  lu.assertNotNil(st)
  lu.assertTrue(unix.S_ISLNK(st:mode()))

  local f = io.open(link, "r")
  lu.assertNotNil(f)
  local content = f:read("*a")
  f:close()
  lu.assertEquals(content, "new content")

  unix.rmrf(tmp)
end

function test_update_symlink_fails_on_regular_file()
  local tmp = make_temp_dir()
  local target = path.join(tmp, "target")
  cosmo.Barf(target, "target")
  local existing_file = path.join(tmp, "existing")
  cosmo.Barf(existing_file, "regular file")

  local ok, err = home.update_symlink(existing_file, target, {})
  lu.assertFalse(ok)
  lu.assertStrContains(err, "not a symlink")

  unix.rmrf(tmp)
end

function test_update_symlink_dry_run()
  local tmp = make_temp_dir()
  local target = path.join(tmp, "target")
  cosmo.Barf(target, "target")
  local link = path.join(tmp, "link")
  local stdout = mock_writer()

  local ok = home.update_symlink(link, target, {
    dry_run = true,
    verbose = true,
    stdout = stdout,
  })
  lu.assertTrue(ok)

  local st = unix.stat(link, unix.AT_SYMLINK_NOFOLLOW)
  lu.assertNil(st)

  lu.assertStrContains(stdout:get(), "would link")

  unix.rmrf(tmp)
end

--------------------------------------------------------------------------------
-- Test: cmd_3p
--------------------------------------------------------------------------------
function test_cmd_3p_empty_share()
  local tmp = make_temp_dir()
  local share_dir = path.join(tmp, "share")
  unix.makedirs(share_dir)

  local stdout = mock_writer()
  local stderr = mock_writer()

  local code = home.cmd_3p({}, {
    home = tmp,
    share_dir = share_dir,
    stdout = stdout,
    stderr = stderr,
    dry_run = true,
  })
  lu.assertEquals(code, 0)

  unix.rmrf(tmp)
end

function test_cmd_3p_creates_symlinks()
  local tmp = make_temp_dir()
  local share_dir = path.join(tmp, "share")
  local bin_dir = path.join(tmp, ".local", "bin")

  local tool_dir = path.join(share_dir, "rg", "14.1.1-abcd1234", "bin")
  unix.makedirs(tool_dir)
  cosmo.Barf(path.join(tool_dir, "rg"), "#!/bin/sh\necho rg")

  local stdout = mock_writer()
  local stderr = mock_writer()

  local code = home.cmd_3p({}, {
    home = tmp,
    share_dir = share_dir,
    stdout = stdout,
    stderr = stderr,
  })
  lu.assertEquals(code, 0)

  local link = path.join(bin_dir, "rg")
  local st = unix.stat(link, unix.AT_SYMLINK_NOFOLLOW)
  lu.assertNotNil(st)
  lu.assertTrue(unix.S_ISLNK(st:mode()))

  unix.rmrf(tmp)
end

function test_cmd_3p_list()
  local tmp = make_temp_dir()
  local share_dir = path.join(tmp, "share")

  local tool_dir = path.join(share_dir, "rg", "14.1.1-abcd1234", "bin")
  unix.makedirs(tool_dir)
  cosmo.Barf(path.join(tool_dir, "rg"), "#!/bin/sh\necho rg")

  local stdout = mock_writer()
  local stderr = mock_writer()

  local code = home.cmd_3p({ "list" }, {
    home = tmp,
    share_dir = share_dir,
    stdout = stdout,
    stderr = stderr,
  })
  lu.assertEquals(code, 0)

  lu.assertStrContains(stdout:get(), "rg 14.1.1-abcd1234")

  unix.rmrf(tmp)
end

function test_cmd_3p_dry_run_verbose()
  local tmp = make_temp_dir()
  local share_dir = path.join(tmp, "share")
  local bin_dir = path.join(tmp, ".local", "bin")

  local tool_dir = path.join(share_dir, "rg", "14.1.1-abcd1234", "bin")
  unix.makedirs(tool_dir)
  cosmo.Barf(path.join(tool_dir, "rg"), "#!/bin/sh\necho rg")

  local stdout = mock_writer()
  local stderr = mock_writer()

  local code = home.cmd_3p({}, {
    home = tmp,
    share_dir = share_dir,
    stdout = stdout,
    stderr = stderr,
    dry_run = true,
    verbose = true,
  })
  lu.assertEquals(code, 0)

  lu.assertStrContains(stdout:get(), "would link")

  local link = path.join(bin_dir, "rg")
  local st = unix.stat(link, unix.AT_SYMLINK_NOFOLLOW)
  lu.assertNil(st)

  unix.rmrf(tmp)
end

function test_cmd_3p_nvim_site_symlink()
  local tmp = make_temp_dir()
  local share_dir = path.join(tmp, "share")
  local bin_dir = path.join(tmp, ".local", "bin")

  local nvim_dir = path.join(share_dir, "nvim", "0.12.0-abcd1234")
  local nvim_bin_dir = path.join(nvim_dir, "bin")
  local nvim_site_dir = path.join(nvim_dir, "share", "nvim", "site")
  unix.makedirs(nvim_bin_dir)
  unix.makedirs(nvim_site_dir)
  cosmo.Barf(path.join(nvim_bin_dir, "nvim"), "#!/bin/sh\necho nvim")
  cosmo.Barf(path.join(nvim_site_dir, "marker"), "site")

  local stdout = mock_writer()
  local stderr = mock_writer()

  local code = home.cmd_3p({}, {
    home = tmp,
    share_dir = share_dir,
    stdout = stdout,
    stderr = stderr,
  })
  lu.assertEquals(code, 0)

  local bin_link = path.join(bin_dir, "nvim")
  local st = unix.stat(bin_link, unix.AT_SYMLINK_NOFOLLOW)
  lu.assertNotNil(st)
  lu.assertTrue(unix.S_ISLNK(st:mode()))

  local site_link = path.join(share_dir, "nvim", "site")
  st = unix.stat(site_link, unix.AT_SYMLINK_NOFOLLOW)
  lu.assertNotNil(st)
  lu.assertTrue(unix.S_ISLNK(st:mode()))

  local marker = path.join(site_link, "marker")
  local f = io.open(marker, "r")
  lu.assertNotNil(f, "symlink should resolve to actual directory")
  local content = f:read("*a")
  f:close()
  lu.assertEquals(content, "site")

  unix.rmrf(tmp)
end

--------------------------------------------------------------------------------
-- Test: main dispatch for 3p
--------------------------------------------------------------------------------
function test_main_3p()
  local tmp = make_temp_dir()
  local share_dir = path.join(tmp, "share")
  unix.makedirs(share_dir)

  local stdout = mock_writer()
  local stderr = mock_writer()

  local code = home.main({ "3p" }, {
    home = tmp,
    share_dir = share_dir,
    stdout = stdout,
    stderr = stderr,
    dry_run = true,
  })
  lu.assertEquals(code, 0)

  unix.rmrf(tmp)
end

function test_cmd_help_output()
  local stderr = mock_writer()
  local code = home.cmd_help({ stderr = stderr })
  lu.assertEquals(code, 0)
  local output = stderr:get()
  lu.assertStrContains(output, "usage:")
  lu.assertStrContains(output, "list")
  lu.assertStrContains(output, "unpack")
end

--------------------------------------------------------------------------------
-- Test: scan_for_latest_version path format
--------------------------------------------------------------------------------
function test_scan_for_latest_version_path_format()
  local tmp = make_temp_dir()
  local share_dir = tmp
  local tool_dir = path.join(share_dir, "rg", "14.1.1-abcd1234")
  local tool_bin_dir = path.join(tool_dir, "bin")
  unix.makedirs(tool_bin_dir)
  local bin_path = path.join(tool_bin_dir, "rg")
  cosmo.Barf(bin_path, "#!/bin/sh\necho rg")

  local result = home.scan_for_latest_version("rg", share_dir)
  lu.assertNotNil(result)
  lu.assertEquals(result.version, "14.1.1")
  lu.assertEquals(result.sha, "abcd1234")
  lu.assertEquals(result.path, bin_path)

  -- Verify the version directory path format is <version>-<sha>, not 0.0.0- or similar
  lu.assertStrContains(result.path, "14.1.1-abcd1234")
  lu.assertNotStrContains(result.path, "0.0.0-")

  unix.rmrf(tmp)
end

function test_scan_for_latest_version_rejects_invalid_paths()
  local tmp = make_temp_dir()
  local share_dir = tmp

  -- Create directory with invalid version format (e.g., 0.0.0-)
  local bad_dir = path.join(share_dir, "rg", "0.0.0-")
  unix.makedirs(bad_dir)
  cosmo.Barf(path.join(bad_dir, "rg"), "#!/bin/sh\necho rg")

  -- Should not find this as a valid version
  local result = home.scan_for_latest_version("rg", share_dir)
  lu.assertNil(result)

  unix.rmrf(tmp)
end

--------------------------------------------------------------------------------
-- Test: detect_platform
--------------------------------------------------------------------------------
function test_detect_platform_returns_string()
  local platform, err = home.detect_platform()
  lu.assertNotNil(platform, "detect_platform failed: " .. tostring(err))
  lu.assertTrue(type(platform) == "string")
  -- Platform should be one of the known values
  lu.assertTrue(
    platform == "darwin-arm64" or platform == "linux-arm64" or platform == "linux-x86_64",
    "unexpected platform: " .. platform
  )
end

os.exit(lu.LuaUnit.run())
