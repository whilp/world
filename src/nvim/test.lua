local script_path = debug.getinfo(1, "S").source:sub(2)
local script_dir = script_path:match("(.+)/[^/]+$")
if script_dir then
  package.path = script_dir .. "/../../.local/lib/lua/?.lua;" .. package.path
else
  package.path = "../../.local/lib/lua/?.lua;" .. package.path
end

local cosmo = require('cosmo')
local unix = cosmo.unix

if script_dir then
  package.path = script_dir .. "/../?.lua;" .. package.path
else
  package.path = "../?.lua;" .. package.path
end

local nvim = require("nvim.main")

function test_load_zsh_environment_returns_table()
  local env = nvim.load_zsh_environment()

  lu.assertTrue(type(env) == "table", "should return a table")
end

function test_load_zsh_environment_completes_without_error()
  local ok, env = pcall(nvim.load_zsh_environment)

  lu.assertTrue(ok, "should complete without error")
  lu.assertTrue(type(env) == "table", "should return a table even if empty")
end

function test_setup_nvim_environment_sets_server_mode()
  local env = nvim.setup_nvim_environment()

  lu.assertEquals(env.NVIM_SERVER_MODE, "1", "should set NVIM_SERVER_MODE to 1")
end

function test_setup_nvim_environment_preserves_loaded_env()
  local env = nvim.setup_nvim_environment()

  lu.assertTrue(env.NVIM_SERVER_MODE ~= nil, "should have NVIM_SERVER_MODE")
  for k, v in pairs(env) do
    lu.assertTrue(type(k) == "string", "env keys should be strings")
    lu.assertTrue(type(v) == "string", "env values should be strings")
  end
end

function test_setup_nvim_environment_returns_table()
  local env = nvim.setup_nvim_environment()

  lu.assertTrue(type(env) == "table", "should return a table")
end

function test_get_script_dir_returns_string()
  local dir = nvim.get_script_dir()

  lu.assertTrue(type(dir) == "string" or dir == nil, "should return a string or nil")
end

function test_resolve_nvim_bin_returns_path()
  local bin = nvim.resolve_nvim_bin()

  lu.assertTrue(type(bin) == "string", "should return a string path")
  lu.assertTrue(bin:match("nvim$") ~= nil, "should end with 'nvim'")
end

function test_resolve_nvim_bin_uses_relative_path()
  local bin = nvim.resolve_nvim_bin()

  lu.assertTrue(bin:match("share/nvim/bin/nvim") ~= nil, "should contain share/nvim/bin/nvim")
end
