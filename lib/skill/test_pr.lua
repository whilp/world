#!/usr/bin/env run-test.lua
local lu = require("luaunit")
local pr = require("skill.pr")
local cosmo = require("cosmo")

-- Test parsing functionality
TestParsing = {}

function TestParsing:test_basic_title_and_body()
  local content = [[# My PR title

This is the body.
]]
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "My PR title")
  lu.assertEquals(result.body, "This is the body.")
end

function TestParsing:test_multiline_body()
  local content = [[# Feature: add new thing

This PR adds a new feature.

## Changes
- Added foo
- Fixed bar

## Testing
Tested manually.
]]
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "Feature: add new thing")
  lu.assertStrContains(result.body, "This PR adds a new feature.")
  lu.assertStrContains(result.body, "## Changes")
  lu.assertStrContains(result.body, "- Added foo")
end

function TestParsing:test_title_only()
  local content = "# Just a title"
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "Just a title")
  lu.assertEquals(result.body, "")
end

function TestParsing:test_title_with_trailing_whitespace()
  local content = "#   Spaced title   \n\nbody"
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "Spaced title")
  lu.assertEquals(result.body, "body")
end

function TestParsing:test_empty_body()
  local content = "# Title\n\n"
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "Title")
  lu.assertEquals(result.body, "")
end

function TestParsing:test_no_title()
  local content = "No hash mark here"
  local result, err = pr.parse_pr_md(content)
  lu.assertNil(result)
  lu.assertStrContains(err, "no title found")
end

function TestParsing:test_empty_content()
  local result, err = pr.parse_pr_md("")
  lu.assertNil(result)
  lu.assertStrContains(err, "no title found")
end

function TestParsing:test_title_with_special_chars()
  local content = "# feat(api): add endpoint for /users\n\nDetails here."
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "feat(api): add endpoint for /users")
  lu.assertEquals(result.body, "Details here.")
end

-- Test GitHub API interactions
TestGithubAPI = {}

function TestGithubAPI:test_successful_get()
  local mock_fetch = function(url, opts)
    lu.assertStrContains(url, "/repos/owner/repo/pulls")
    lu.assertEquals(opts.method, "GET")
    lu.assertStrContains(opts.headers["Authorization"], "Bearer ")
    return 200, {}, cosmo.EncodeJson({number = 123})
  end

  local status, data = pr.github_request("GET", "/repos/owner/repo/pulls", "token", nil, {fetch = mock_fetch})
  lu.assertEquals(status, 200)
  lu.assertEquals(data.number, 123)
end

function TestGithubAPI:test_fetch_failure()
  local mock_fetch = function()
    return nil, "connection refused"
  end

  local status, err = pr.github_request("GET", "/test", "token", nil, {fetch = mock_fetch})
  lu.assertNil(status)
  lu.assertStrContains(err, "fetch failed")
end

function TestGithubAPI:test_finds_pr()
  local mock_fetch = function()
    return 200, {}, cosmo.EncodeJson({{number = 42, title = "Test PR"}})
  end

  local pr_num = pr.find_pr_number("owner", "repo", "branch", "token", {fetch = mock_fetch})
  lu.assertEquals(pr_num, 42)
end

function TestGithubAPI:test_no_pr_found()
  local mock_fetch = function()
    return 200, {}, cosmo.EncodeJson({})
  end

  local pr_num, err = pr.find_pr_number("owner", "repo", "branch", "token", {fetch = mock_fetch})
  lu.assertNil(pr_num)
  lu.assertStrContains(err, "no open PR found")
end

function TestGithubAPI:test_update_pr_success()
  local captured_body
  local mock_fetch = function(_url, opts)
    captured_body = opts.body
    return 200, {}, cosmo.EncodeJson({number = 42, title = "New Title"})
  end

  local ok = pr.update_pr("owner", "repo", 42, "New Title", "New body", "token", {fetch = mock_fetch})
  lu.assertTrue(ok)
  lu.assertStrContains(captured_body, "New Title")
  lu.assertStrContains(captured_body, "New body")
end

function TestGithubAPI:test_update_pr_api_error()
  local mock_fetch = function()
    return 403, {}, cosmo.EncodeJson({message = "Forbidden"})
  end

  local ok, err = pr.update_pr("owner", "repo", 42, "Title", "Body", "token", {fetch = mock_fetch})
  lu.assertNil(ok)
  lu.assertStrContains(err, "403")
  lu.assertStrContains(err, "Forbidden")
end

-- Test Claude Code CLI environment
--
-- Claude Code environment characteristics:
-- 1. Git repository with proxied remote (http://local_proxy@127.0.0.1/git/owner/repo)
-- 2. No GITHUB_* environment variables set
-- 3. HTTP/HTTPS proxy with JWT authentication
-- 4. Proxy intercepts GitHub API requests and adds auth automatically
-- 5. Unauthenticated API calls succeed via proxy
--
-- Code flow in this environment:
-- - get_current_branch() -> uses git (mocked)
-- - get_git_info() -> parses /git/owner/repo pattern (mocked)
-- - find_pr_for_branch() -> makes unauthenticated API call (proxy adds auth)
TestClaudeRemote = {}

function TestClaudeRemote:test_happy_path()
  -- mock git state for claude code environment
  local mock_owner = "whilp"
  local mock_repo = "world"
  local mock_branch = "claude/extract-prla-fixes-Xch0u"

  -- simulate claude code environment:
  -- - no GITHUB_* env vars
  -- - git operations return mock values
  -- - unauthenticated API call succeeds (proxy adds auth)

  -- mock cosmo.Fetch to simulate proxy behavior
  local original_fetch = cosmo.Fetch
  cosmo.Fetch = function(url, opts)
    -- verify this is an unauthenticated request (no auth header)
    lu.assertNil(opts.headers["Authorization"], "should be unauthenticated")

    -- verify correct GitHub API URL
    lu.assertStrContains(url, "https://api.github.com")
    lu.assertStrContains(url, string.format("/repos/%s/%s/pulls", mock_owner, mock_repo))
    lu.assertStrContains(url, string.format("head=%s:%s", mock_owner, mock_branch))

    -- simulate proxy authenticating and returning successful response
    return 200, {}, cosmo.EncodeJson({{number = 209}})
  end

  -- test the code flow: find_pr_for_branch with mocked git state
  local pr_number = pr.find_pr_for_branch(mock_owner, mock_repo, mock_branch)

  -- restore
  cosmo.Fetch = original_fetch

  lu.assertNotNil(pr_number, "should find PR via unauthenticated API call")
  lu.assertEquals(pr_number, 209)
end

-- Test GitHub Actions environment
TestGithubAction = {}

function TestGithubAction:test_github_actions_with_token_and_pr_number()
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

  lu.assertNotNil(pr_number)
  lu.assertEquals(pr_number, 42)
end

function TestGithubAction:test_github_actions_missing_token()
  local mock_env = {
    GITHUB_ACTIONS = "true",
    GITHUB_REPOSITORY = "owner/repo",
  }
  local mock_getenv = function(key) return mock_env[key] end

  local code, msg = pr.main({getenv = mock_getenv})
  lu.assertEquals(code, 1)
  lu.assertStrContains(msg, "GITHUB_TOKEN")
end

function TestGithubAction:test_github_actions_missing_repository()
  local mock_env = {
    GITHUB_ACTIONS = "true",
    GITHUB_TOKEN = "test-token",
  }
  local mock_getenv = function(key) return mock_env[key] end

  local code, msg = pr.main({getenv = mock_getenv})
  lu.assertEquals(code, 1)
  lu.assertStrContains(msg, "GITHUB_REPOSITORY")
end
