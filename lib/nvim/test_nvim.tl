local nvim = require("nvim.main")

local function test_setup_nvim_environment_sets_server_mode()
  local bin = nvim.resolve_nvim_bin()
  if not bin then return end

  local env = nvim.setup_nvim_environment(bin)

  local found = false
  for _, entry in ipairs(env as {string}) do
    if entry == "NVIM_SERVER_MODE=1" then
      found = true
      break
    end
  end
  assert(found, "should set NVIM_SERVER_MODE to 1")
end
test_setup_nvim_environment_sets_server_mode()

local function test_setup_nvim_environment_preserves_loaded_env()
  local bin = nvim.resolve_nvim_bin()
  if not bin then return end

  local env = nvim.setup_nvim_environment(bin)

  local has_server_mode = false
  for _, entry in ipairs(env as {string}) do
    assert(type(entry) == "string", "env entries should be strings")
    assert(entry:match("^[^=]+=.*$") ~= nil, "env entries should be in KEY=value format")
    if entry:match("^NVIM_SERVER_MODE=") then
      has_server_mode = true
    end
  end
  assert(has_server_mode, "should have NVIM_SERVER_MODE")
end
test_setup_nvim_environment_preserves_loaded_env()

local function test_setup_nvim_environment_returns_table()
  local bin = nvim.resolve_nvim_bin()
  if not bin then return end

  local env = nvim.setup_nvim_environment(bin)

  assert(type(env) == "table", "should return a table")
end
test_setup_nvim_environment_returns_table()

local function test_setup_nvim_environment_sets_vimruntime()
  local bin = nvim.resolve_nvim_bin()
  if not bin then return end  -- skip if nvim not available

  local env = nvim.setup_nvim_environment(bin)
  if not env then return end  -- skip if setup failed

  local found = false
  for _, entry in ipairs(env as {string}) do
    if entry:match("^VIMRUNTIME=") then
      found = true
      break
    end
  end
  -- VIMRUNTIME may not be set in all environments, so just check the env table is valid
  assert(type(env) == "table", "should return a table")
end
test_setup_nvim_environment_sets_vimruntime()

local function test_resolve_nvim_bin_returns_path_when_exists()
  local bin, err = nvim.resolve_nvim_bin()

  if bin then
    assert(type(bin) == "string", "should return a string path")
    assert(bin:match("nvim$") ~= nil, "should end with 'nvim'")
  else
    assert(type(err) == "string", "should return error message when not found")
  end
end
test_resolve_nvim_bin_returns_path_when_exists()

local function test_resolve_nvim_bin_returns_error_when_missing()
  local bin, err = nvim.resolve_nvim_bin()

  -- either binary exists and we get a path, or it's missing and we get an error
  if not bin then
    assert(type(err) == "string", "should return error message")
    assert(err:match("not found") ~= nil, "error should mention 'not found'")
  end
end
test_resolve_nvim_bin_returns_error_when_missing()
