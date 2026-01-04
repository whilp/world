#!/usr/bin/env run-test.lua
-- teal ignore: test file

local pr = require("skill.pr")
local cosmo = require("cosmo")

local function test_github_actions_with_token_and_pr_number()
  local mock_fetch = function()
    return 200, {}, cosmo.EncodeJson({number = 42})
  end

  local mock_env = {
    GITHUB_ACTIONS = "true",
    GITHUB_TOKEN = "test-token",
    GITHUB_REPOSITORY = "owner/repo",
    GITHUB_PR_NUMBER = "42",
  }
  local mock_getenv = function(key) return mock_env[key] end

  local pr_number = pr.get_pr_number_from_env({
    fetch = mock_fetch,
    getenv = mock_getenv,
  })

  assert(pr_number, "expected PR number")
  assert(pr_number == 42, "expected PR number 42")
end
test_github_actions_with_token_and_pr_number()

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
