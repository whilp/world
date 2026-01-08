#!/usr/bin/env lua
-- sandbox.lua: centralized unveil constraints for build phases
-- usage: sandbox.lua <profile> <script> [args...]

local unix = require("cosmo.unix")
local path = require("cosmo.path")

-- profile definitions: each profile specifies paths to unveil
-- paths can be:
--   string: unveil with that permission
--   function: called with args table, returns path or nil
local profiles = {
  fetch = {
    -- version file passed as arg[1]
    [function(a) return a[1] end] = "r",
    -- output dir derived from arg[3]
    [function(a) return a[3] and path.dirname(a[3]) end] = "rwc",
    -- FETCH_O env var
    [function() return os.getenv("FETCH_O") end] = "rwc",
    -- network requirements
    ["/etc/resolv.conf"] = "r",
    ["/etc/ssl"] = "r",
  },

  stage = {
    -- version file passed as arg[1]
    [function(a) return a[1] end] = "r",
    -- input archive dir passed as arg[3]
    [function(a) return a[3] end] = "r",
    -- output dir derived from arg[4]
    [function(a) return a[4] and path.dirname(a[4]) end] = "rwc",
    -- STAGE_O env var
    [function() return os.getenv("STAGE_O") end] = "rwc",
    -- external tools
    ["/usr/bin"] = "rx",
  },

  test = {
    -- test file passed as arg[1]
    [function(a) return a[1] end] = "r",
    -- TEST_TMPDIR created before unveil
    [function() return os.getenv("TEST_TMPDIR") end] = "rwc",
    -- build outputs
    ["o"] = "rx",
    -- source files
    ["lib"] = "r",
    ["3p"] = "r",
    -- system paths
    ["/dev/null"] = "rw",
    ["/proc"] = "r",
    ["/usr"] = "rx",
    ["/etc"] = "r",
    -- ape loaders (handled specially)
    ["$HOME/.ape-*"] = "rx",
  },
}

local function setup_ape_loaders()
  local home = os.getenv("HOME")
  if not home then return end
  local dir = unix.opendir(home)
  if not dir then return end
  for name in dir do
    if name:match("^%.ape%-") then
      unix.unveil(path.join(home, name), "rx")
    end
  end
end

local function create_test_tmpdir()
  local tmpdir = unix.mkdtemp("/tmp/test_XXXXXX")
  unix.setenv("TEST_TMPDIR", tmpdir)
  -- also set global for test compatibility
  _G.TEST_TMPDIR = tmpdir
  return tmpdir
end

local function apply_profile(profile_name, script_args)
  local profile = profiles[profile_name]
  if not profile then
    io.stderr:write("sandbox: unknown profile: " .. profile_name .. "\n")
    os.exit(1)
  end

  -- special handling for test profile: create tmpdir first
  if profile_name == "test" then
    create_test_tmpdir()
    _G.TEST_DIR = os.getenv("TEST_DIR")
  end

  -- collect paths to unveil
  local unveils = {}
  for path_spec, perm in pairs(profile) do
    local p
    if type(path_spec) == "function" then
      p = path_spec(script_args)
    elseif path_spec == "$HOME/.ape-*" then
      -- handled separately
      p = nil
    else
      p = path_spec
    end
    if p and p ~= "" then
      -- ensure directory exists for rwc paths before unveiling
      if perm:match("w") then
        unix.makedirs(p)
      end
      table.insert(unveils, {path = p, perm = perm})
    end
  end

  -- apply unveils
  for _, u in ipairs(unveils) do
    local ok, err = unix.unveil(u.path, u.perm)
    if not ok then
      io.stderr:write("sandbox: unveil failed for " .. u.path .. ": " .. tostring(err) .. "\n")
      os.exit(1)
    end
  end

  -- handle ape loaders for test profile
  if profile["$HOME/.ape-*"] then
    setup_ape_loaders()
  end

  -- lock down
  unix.unveil(nil, nil)
end

local function main(args)
  if #args < 2 then
    io.stderr:write("usage: sandbox.lua <profile> <script> [args...]\n")
    io.stderr:write("profiles: fetch, stage, test\n")
    os.exit(1)
  end

  local profile_name = args[1]
  local script = args[2]
  local script_args = {}
  for i = 3, #args do
    script_args[i - 2] = args[i]
  end

  -- load script BEFORE applying sandbox (script path may not be unveiled)
  local chunk, load_err = loadfile(script)
  if not chunk then
    io.stderr:write("sandbox: failed to load " .. script .. ": " .. tostring(load_err) .. "\n")
    os.exit(1)
  end

  -- set up arg table for the script
  _G.arg = script_args
  -- mark as running in sandbox (scripts check this instead of cosmo.is_main())
  _G.SANDBOX_MAIN = true

  -- apply sandbox
  apply_profile(profile_name, script_args)

  -- run the loaded script
  local ok, err = pcall(chunk, table.unpack(script_args))
  if not ok then
    io.stderr:write("sandbox: " .. tostring(err) .. "\n")
    os.exit(1)
  end
end

main(arg)
