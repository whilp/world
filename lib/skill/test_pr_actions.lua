#!/usr/bin/env run-test.lua
-- teal ignore: test file

local pr = require("skill.pr")
local cosmo = require("cosmo")

local function test_github_actions_missing_token()
  local mock_env = {
    GITHUB_ACTIONS = "true",
    GITHUB_REPOSITORY = "owner/repo",
  }
  local mock_getenv = function(key) return mock_env[key] end

  local code, msg = pr.main({getenv = mock_getenv})
  assert(code == 1, "expected exit code 1")
  assert(msg and msg:match("GITHUB_TOKEN"), "expected GITHUB_TOKEN error")
end
test_github_actions_missing_token()

local function test_github_actions_missing_repository()
  local mock_env = {
    GITHUB_ACTIONS = "true",
    GITHUB_TOKEN = "test-token",
  }
  local mock_getenv = function(key) return mock_env[key] end

  local code, msg = pr.main({getenv = mock_getenv})
  assert(code == 1, "expected exit code 1")
  assert(msg and msg:match("GITHUB_REPOSITORY"), "expected GITHUB_REPOSITORY error")
end
test_github_actions_missing_repository()

local function test_github_actions_missing_pr_number()
  local mock_env = {
    GITHUB_ACTIONS = "true",
    GITHUB_TOKEN = "test-token",
    GITHUB_REPOSITORY = "owner/repo",
  }
  local mock_getenv = function(key) return mock_env[key] end

  local code, msg = pr.main({getenv = mock_getenv})
  assert(code == 1, "expected exit code 1")
  assert(msg and msg:match("GITHUB_PR_NUMBER"), "expected GITHUB_PR_NUMBER error")
end
test_github_actions_missing_pr_number()
