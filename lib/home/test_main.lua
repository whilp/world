local lu = require("luaunit")
local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local home = require("home.main")

-- Helper: create a test subdirectory in TEST_TMPDIR
local test_counter = 0
local function make_test_dir()
  test_counter = test_counter + 1
  local dir = path.join(TEST_TMPDIR, "home_test_" .. test_counter)
  unix.makedirs(dir)
  return dir
end

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
  local tmp = make_test_dir()
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
  local tmp = make_test_dir()
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
  local tmp = make_test_dir()
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
  local tmp = make_test_dir()
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
  local tmp = make_test_dir()
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
  local tmp = make_test_dir()
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
  local tmp = make_test_dir()
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
  local tmp = make_test_dir()
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
  local tmp = make_test_dir()
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
  local tmp = make_test_dir()
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
  local tmp = make_test_dir()
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
  local tmp = make_test_dir()
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
  local tmp = make_test_dir()
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
  local tmp = make_test_dir()
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
-- Test: 3p binaries in .local/share structure
--------------------------------------------------------------------------------
function test_unpack_3p_binary_structure()
  local tmp = make_test_dir()
  local zip_root = path.join(tmp, "zip/")

  -- Create versioned binary structure
  local nvim_bin_dir = path.join(zip_root, ".local/share/nvim/0.12.0-abcd1234/bin")
  unix.makedirs(nvim_bin_dir)
  cosmo.Barf(path.join(nvim_bin_dir, "nvim"), "#!/bin/sh\necho nvim")

  local rg_bin_dir = path.join(zip_root, ".local/share/rg/14.1.1-efgh5678/bin")
  unix.makedirs(rg_bin_dir)
  cosmo.Barf(path.join(rg_bin_dir, "rg"), "#!/bin/sh\necho rg")

  local manifest = {
    files = {
      [".local/share/nvim/0.12.0-abcd1234/bin/nvim"] = { mode = 493 },
      [".local/share/rg/14.1.1-efgh5678/bin/rg"] = { mode = 493 },
    },
  }

  local dest = path.join(tmp, "dest")
  local code = home.cmd_unpack(dest, false, {
    manifest = manifest,
    zip_root = zip_root,
  })

  lu.assertEquals(code, 0)

  -- Verify binaries were extracted with correct structure
  local nvim_path = path.join(dest, ".local/share/nvim/0.12.0-abcd1234/bin/nvim")
  local st = unix.stat(nvim_path)
  lu.assertNotNil(st, "nvim binary should exist")
  local mode_bits = st:mode() & 0x1FF
  lu.assertEquals(mode_bits, tonumber("755", 8), "nvim should be executable")

  local rg_path = path.join(dest, ".local/share/rg/14.1.1-efgh5678/bin/rg")
  st = unix.stat(rg_path)
  lu.assertNotNil(st, "rg binary should exist")
  mode_bits = st:mode() & 0x1FF
  lu.assertEquals(mode_bits, tonumber("755", 8), "rg should be executable")

  unix.rmrf(tmp)
end

function test_list_includes_3p_binaries()
  local manifest = {
    files = {
      [".zshrc"] = { mode = 420 },
      [".local/share/nvim/0.12.0-abcd1234/bin/nvim"] = { mode = 493 },
      [".local/share/rg/14.1.1-efgh5678/bin/rg"] = { mode = 493 },
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
  lu.assertStrContains(output, ".local/share/nvim/0.12.0-abcd1234/bin/nvim")
  lu.assertStrContains(output, ".local/share/rg/14.1.1-efgh5678/bin/rg")
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
