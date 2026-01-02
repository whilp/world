local lu = require("luaunit")
local pr = require("build.pr")

TestParsePrMd = {}

function TestParsePrMd:test_basic_title_and_body()
  local content = [[# My PR title

This is the body.
]]
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "My PR title")
  lu.assertEquals(result.body, "This is the body.")
end

function TestParsePrMd:test_multiline_body()
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

function TestParsePrMd:test_title_only()
  local content = "# Just a title"
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "Just a title")
  lu.assertEquals(result.body, "")
end

function TestParsePrMd:test_title_with_trailing_whitespace()
  local content = "#   Spaced title   \n\nbody"
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "Spaced title")
  lu.assertEquals(result.body, "body")
end

function TestParsePrMd:test_empty_body()
  local content = "# Title\n\n"
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "Title")
  lu.assertEquals(result.body, "")
end

function TestParsePrMd:test_no_title()
  local content = "No hash mark here"
  local result, err = pr.parse_pr_md(content)
  lu.assertNil(result)
  lu.assertStrContains(err, "no title found")
end

function TestParsePrMd:test_empty_content()
  local result, err = pr.parse_pr_md("")
  lu.assertNil(result)
  lu.assertStrContains(err, "no title found")
end

function TestParsePrMd:test_title_with_special_chars()
  local content = "# feat(api): add endpoint for /users\n\nDetails here."
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "feat(api): add endpoint for /users")
  lu.assertEquals(result.body, "Details here.")
end

local cosmo = require("cosmo")

TestGithubRequest = {}

function TestGithubRequest:test_successful_get()
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

function TestGithubRequest:test_fetch_failure()
  local mock_fetch = function()
    return nil, "connection refused"
  end

  local status, err = pr.github_request("GET", "/test", "token", nil, {fetch = mock_fetch})
  lu.assertNil(status)
  lu.assertStrContains(err, "fetch failed")
end

TestFindPrNumber = {}

function TestFindPrNumber:test_finds_pr()
  local mock_fetch = function()
    return 200, {}, cosmo.EncodeJson({{number = 42, title = "Test PR"}})
  end

  local pr_num = pr.find_pr_number("owner", "repo", "branch", "token", {fetch = mock_fetch})
  lu.assertEquals(pr_num, 42)
end

function TestFindPrNumber:test_no_pr_found()
  local mock_fetch = function()
    return 200, {}, cosmo.EncodeJson({})
  end

  local pr_num, err = pr.find_pr_number("owner", "repo", "branch", "token", {fetch = mock_fetch})
  lu.assertNil(pr_num)
  lu.assertStrContains(err, "no open PR found")
end

TestUpdatePr = {}

function TestUpdatePr:test_successful_update()
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

function TestUpdatePr:test_api_error()
  local mock_fetch = function()
    return 403, {}, cosmo.EncodeJson({message = "Forbidden"})
  end

  local ok, err = pr.update_pr("owner", "repo", 42, "Title", "Body", "token", {fetch = mock_fetch})
  lu.assertNil(ok)
  lu.assertStrContains(err, "403")
  lu.assertStrContains(err, "Forbidden")
end

local environ = require("environ")

TestGetCurrentBranch = {}

function TestGetCurrentBranch:test_returns_branch_name()
  local branch = pr.get_current_branch()
  -- should return a string when in a git repo
  lu.assertNotNil(branch)
  lu.assertTrue(#branch > 0)
end

TestGetPrNumberFromEnv = {}

function TestGetPrNumberFromEnv:test_falls_back_to_git_branch()
  -- when GITHUB_HEAD_REF/GITHUB_REF_NAME not set, should use git branch
  local mock_fetch = function()
    return 200, {}, cosmo.EncodeJson({{number = 207}})
  end

  -- create env object with required vars but no branch vars
  local env = environ.new({
    "GITHUB_TOKEN=test-token",
    "GITHUB_REPOSITORY=owner/repo",
  })

  local pr_num, err = pr.get_pr_number_from_env({
    fetch = mock_fetch,
    env = env,
  })

  if pr_num then
    lu.assertEquals(pr_num, 207)
  else
    -- if we're not in a git repo, that's ok for this test
    lu.assertStrContains(err, "branch")
  end
end

TestMain = {}

function TestMain:test_missing_token_returns_error()
  local empty_env = environ.new({})
  local code, msg = pr.main({env = empty_env})
  lu.assertEquals(code, 1)
  lu.assertStrContains(msg, "GITHUB_TOKEN")
end

os.exit(lu.LuaUnit.run())
