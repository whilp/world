#!/usr/bin/env run-test.lua
-- teal ignore: test file

local hooks = require("skill.hooks")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local tmpdir = os.getenv("TEST_TMPDIR") or "/tmp"

--------------------------------------------------------------------------------
-- serialize tests
--------------------------------------------------------------------------------

local function test_load_config_empty()
  -- test loading from non-existent file returns empty table
  local config_path = path.join(tmpdir, "nonexistent_hooks.lua")
  local config = hooks.load_config(config_path)
  assert(type(config) == "table", "expected table")
  assert(next(config) == nil, "expected empty table")
end
test_load_config_empty()

local function test_save_and_load_config()
  local config_path = path.join(tmpdir, "test_hooks.lua")

  -- save config
  local config = {
    session_start_bootstrap = true,
    session_start_make_help = false,
    stop_check_pr_file = {enabled = true, strict = false},
  }
  local ok, err = hooks.save_config(config, config_path)
  assert(ok, "save failed: " .. tostring(err))

  -- load config
  local loaded = hooks.load_config(config_path)
  assert(loaded.session_start_bootstrap == true, "expected bootstrap true")
  assert(loaded.session_start_make_help == false, "expected make_help false")
  assert(type(loaded.stop_check_pr_file) == "table", "expected stop_check_pr_file table")
  assert(loaded.stop_check_pr_file.enabled == true, "expected enabled true")
  assert(loaded.stop_check_pr_file.strict == false, "expected strict false")

  -- cleanup
  os.remove(config_path)
end
test_save_and_load_config()

--------------------------------------------------------------------------------
-- is_enabled tests
--------------------------------------------------------------------------------

local function test_is_enabled()
  local config = {
    enabled_bool = true,
    disabled_bool = false,
    enabled_table = {enabled = true, option = "value"},
    disabled_table = {enabled = false, option = "value"},
    implicit_enabled = {option = "value"},
  }

  assert(hooks.is_enabled(config, "enabled_bool"), "expected enabled_bool enabled")
  assert(not hooks.is_enabled(config, "disabled_bool"), "expected disabled_bool disabled")
  assert(hooks.is_enabled(config, "enabled_table"), "expected enabled_table enabled")
  assert(not hooks.is_enabled(config, "disabled_table"), "expected disabled_table disabled")
  assert(hooks.is_enabled(config, "implicit_enabled"), "expected implicit_enabled enabled")
  assert(not hooks.is_enabled(config, "not_present"), "expected not_present disabled")
end
test_is_enabled()

--------------------------------------------------------------------------------
-- get_handler_config tests
--------------------------------------------------------------------------------

local function test_get_handler_config()
  local config = {
    with_options = {enabled = true, strict = true, timeout = 30},
    bool_only = true,
  }

  local opts = hooks.get_handler_config(config, "with_options")
  assert(opts.strict == true, "expected strict option")
  assert(opts.timeout == 30, "expected timeout option")

  opts = hooks.get_handler_config(config, "bool_only")
  assert(next(opts) == nil, "expected empty options for bool config")
end
test_get_handler_config()

--------------------------------------------------------------------------------
-- available_handlers tests
--------------------------------------------------------------------------------

local function test_available_handlers()
  assert(hooks.available_handlers.session_start_bootstrap, "expected session_start_bootstrap")
  assert(hooks.available_handlers.session_start_make_help, "expected session_start_make_help")
  assert(hooks.available_handlers.post_commit_pr_reminder, "expected post_commit_pr_reminder")
  assert(hooks.available_handlers.stop_check_pr_file, "expected stop_check_pr_file")

  -- check structure
  local handler = hooks.available_handlers.session_start_bootstrap
  assert(handler.description, "expected description")
  assert(handler.event == "SessionStart", "expected event")
end
test_available_handlers()

--------------------------------------------------------------------------------
-- integration tests (with temp directory)
--------------------------------------------------------------------------------

local function test_config_roundtrip()
  local config_dir = path.join(tmpdir, ".claude")
  unix.makedirs(config_dir, tonumber("755", 8))
  local config_path = path.join(config_dir, "hooks.lua")

  -- test full roundtrip
  local original = {
    session_start_bootstrap = true,
    stop_check_pr_file = {enabled = true, strict = false},
  }

  local ok, err = hooks.save_config(original, config_path)
  assert(ok, "save failed: " .. tostring(err))

  local loaded = hooks.load_config(config_path)
  assert(loaded.session_start_bootstrap == true, "bootstrap mismatch")
  assert(loaded.stop_check_pr_file.strict == false, "strict mismatch")

  -- cleanup
  os.remove(config_path)
  unix.rmrf(config_dir)
end
test_config_roundtrip()
