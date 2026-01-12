#!/usr/bin/env run-test.lua
-- test skill module integration with cosmic binary

local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local cosmic = path.join(os.getenv("TEST_BIN"), "cosmic")

-- helper to create clean environment without LUA_PATH
-- this ensures we test the bundled libraries, not the repo
local function clean_env(): {string}
  local env: {string} = {}
  for _, line in ipairs(unix.environ()) do
    if not line:match("^LUA_PATH=") and not line:match("^LUA_CPATH=") then
      env[#env + 1] = line
    end
  end
  return env
end

local function test_skill_loads()
  local ok, out = spawn({cosmic, "-l", "skill", "-e", "print('loaded')"}, {env = clean_env()}):read()
  assert(ok, "cosmic -l skill failed to load")
  assert((out as string):find("loaded", 1, true), "expected output to contain 'loaded'")
end
test_skill_loads()

local function test_skill_pr_outside_actions()
  -- when not in GitHub Actions, should show help
  local env = clean_env()
  env[#env + 1] = "GITHUB_ACTIONS=false"

  local ok, out = spawn({cosmic, "--skill", "pr"}, { env = env }):read()
  assert(ok, "skill pr should not error outside GitHub Actions")
  assert((out as string):find("Updates PR title and description", 1, true), "expected help text in output")
end
test_skill_pr_outside_actions()

local function test_skill_pr_requires_token()
  -- in GitHub Actions without token, should fail
  local env = clean_env()
  env[#env + 1] = "GITHUB_ACTIONS=true"
  env[#env + 1] = "GITHUB_REPOSITORY=owner/repo"
  env[#env + 1] = "GITHUB_PR_NUMBER=123"
  -- explicitly unset GITHUB_TOKEN if it exists
  for i = #env, 1, -1 do
    if env[i]:match("^GITHUB_TOKEN=") then
      table.remove(env, i)
    end
  end

  local ok, out, exit_code = spawn({cosmic, "--skill", "pr"}, { env = env }):read()
  assert(not ok, "skill pr should fail without GITHUB_TOKEN")
  assert(exit_code == 1, "expected exit code 1, got " .. tostring(exit_code))
end
test_skill_pr_requires_token()
