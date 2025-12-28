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

  local found = false
  for _, entry in ipairs(env) do
    if entry == "NVIM_SERVER_MODE=1" then
      found = true
      break
    end
  end
  lu.assertTrue(found, "should set NVIM_SERVER_MODE to 1")
end

function test_setup_nvim_environment_preserves_loaded_env()
  local env = nvim.setup_nvim_environment()

  local has_server_mode = false
  for _, entry in ipairs(env) do
    lu.assertTrue(type(entry) == "string", "env entries should be strings")
    lu.assertTrue(entry:match("^[^=]+=.*$") ~= nil, "env entries should be in KEY=value format")
    if entry:match("^NVIM_SERVER_MODE=") then
      has_server_mode = true
    end
  end
  lu.assertTrue(has_server_mode, "should have NVIM_SERVER_MODE")
end

function test_setup_nvim_environment_returns_table()
  local env = nvim.setup_nvim_environment()

  lu.assertTrue(type(env) == "table", "should return a table")
end

function test_resolve_nvim_bin_returns_path()
  local bin = nvim.resolve_nvim_bin()

  lu.assertTrue(type(bin) == "string", "should return a string path")
  lu.assertTrue(bin:match("nvim$") ~= nil, "should end with 'nvim'")
end

function test_resolve_nvim_bin_uses_share_path()
  local bin = nvim.resolve_nvim_bin()

  lu.assertTrue(bin:match("share/nvim/") ~= nil, "should contain share/nvim/")
end
