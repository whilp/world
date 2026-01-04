#!/usr/bin/env run-test.lua
-- teal ignore: test file
-- test skill module integration with cosmic binary

local lu = require("luaunit")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local cosmic = path.join(os.getenv("TEST_BIN"), "cosmic")

-- helper to create clean environment without LUA_PATH
-- this ensures we test the bundled libraries, not the repo
local function clean_env()
  local env = {}
  for _, line in ipairs(unix.environ()) do
    if not line:match("^LUA_PATH=") and not line:match("^LUA_CPATH=") then
      env[#env + 1] = line
    end
  end
  return env
end

TestSkill = {}

function TestSkill:test_skill_loads()
  local ok, out = spawn({cosmic, "-l", "skill", "-e", "print('loaded')"}, {env = clean_env()}):read()
  lu.assertTrue(ok, "cosmic -l skill failed to load")
  lu.assertStrContains(out, "loaded")
end

function TestSkill:test_skill_update_pr_outside_actions()
  -- when not in GitHub Actions, should show help
  local env = clean_env()
  env[#env + 1] = "GITHUB_ACTIONS=false"

  local ok, out = spawn({cosmic, "-l", "skill", "update-pr"}, { env = env }):read()
  lu.assertTrue(ok, "skill update-pr should not error outside GitHub Actions")
  lu.assertStrContains(out, "Updates PR title and description")
end

function TestSkill:test_skill_update_pr_requires_token()
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

  local ok, out, exit_code = spawn({cosmic, "-l", "skill", "update-pr"}, { env = env }):read()
  lu.assertFalse(ok, "skill update-pr should fail without GITHUB_TOKEN")
  lu.assertEquals(exit_code, 1)
end
