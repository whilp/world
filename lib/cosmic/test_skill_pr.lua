-- test skill.pr module integration with cosmic binary

local lu = require("luaunit")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local cosmic = path.join(os.getenv("TEST_BIN_DIR"), "bin", "cosmic")

TestSkillPr = {}

function TestSkillPr:test_skill_pr_loads()
  local ok, out = spawn({cosmic, "-l", "skill.pr", "-e", "print('loaded')"}):read()
  lu.assertTrue(ok, "cosmic -l skill.pr failed to load")
  lu.assertStrContains(out, "loaded")
end

function TestSkillPr:test_skill_pr_update_outside_actions()
  -- when not in GitHub Actions, should show help
  local env = unix.environ()
  env[#env + 1] = "GITHUB_ACTIONS=false"

  local ok, out = spawn({cosmic, "-l", "skill.pr", "update"}, { env = env }):read()
  lu.assertTrue(ok, "skill.pr update should not error outside GitHub Actions")
  lu.assertStrContains(out, "Updates PR title and description")
end

function TestSkillPr:test_skill_pr_update_requires_token()
  -- in GitHub Actions without token, should fail
  local env = unix.environ()
  env[#env + 1] = "GITHUB_ACTIONS=true"
  env[#env + 1] = "GITHUB_REPOSITORY=owner/repo"
  env[#env + 1] = "GITHUB_PR_NUMBER=123"
  -- explicitly unset GITHUB_TOKEN if it exists
  for i = #env, 1, -1 do
    if env[i]:match("^GITHUB_TOKEN=") then
      table.remove(env, i)
    end
  end

  local ok, out, exit_code = spawn({cosmic, "-l", "skill.pr", "update"}, { env = env }):read()
  lu.assertFalse(ok, "skill.pr update should fail without GITHUB_TOKEN")
  lu.assertEquals(exit_code, 1)
end
