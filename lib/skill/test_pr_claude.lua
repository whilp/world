#!/usr/bin/env run-test.lua

local pr = require("skill.pr")
local cosmo = require("cosmo")

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

local function test_happy_path()
  local mock_owner = "whilp"
  local mock_repo = "world"
  local mock_branch = "claude/extract-prla-fixes-Xch0u"

  -- mock cosmo.Fetch to simulate proxy behavior
  local original_fetch = cosmo.Fetch
  cosmo.Fetch = function(url, opts)
    -- verify this is an unauthenticated request (no auth header)
    assert(not opts.headers["Authorization"], "should be unauthenticated")

    -- verify correct GitHub API URL
    assert(url:match("https://api.github.com"), "expected GitHub API URL")
    assert(url:match("/repos/whilp/world/pulls"), "expected pulls endpoint")
    assert(url:match("head=whilp:"), "expected head parameter")

    -- simulate proxy authenticating and returning successful response
    return 200, {}, cosmo.EncodeJson({{number = 209}})
  end

  -- test the code flow: find_pr_for_branch with mocked git state
  local pr_number = pr.find_pr_for_branch(mock_owner, mock_repo, mock_branch)

  -- restore
  cosmo.Fetch = original_fetch

  assert(pr_number, "should find PR via unauthenticated API call")
  assert(pr_number == 209, "expected PR number 209")
end
test_happy_path()
