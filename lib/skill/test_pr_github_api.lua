#!/usr/bin/env run-test.lua
-- teal ignore: test file

local pr = require("skill.pr")
local cosmo = require("cosmo")

local function test_successful_get()
  local mock_fetch = function(url, opts)
    assert(url:match("/repos/owner/repo/pulls"), "expected URL to contain /repos/owner/repo/pulls")
    assert(opts.method == "GET", "expected GET method")
    assert(opts.headers["Authorization"]:match("Bearer "), "expected Bearer token")
    return 200, {}, cosmo.EncodeJson({number = 123})
  end

  local status, data = pr.github_request("GET", "/repos/owner/repo/pulls", "token", nil, {fetch = mock_fetch})
  assert(status == 200, "expected status 200")
  assert(data.number == 123, "expected number 123")
end
test_successful_get()

local function test_fetch_failure()
  local mock_fetch = function()
    return nil, "connection refused"
  end

  local status, err = pr.github_request("GET", "/test", "token", nil, {fetch = mock_fetch})
  assert(not status, "expected no status")
  assert(err and err:match("fetch failed"), "expected fetch failed error")
end
test_fetch_failure()

local function test_finds_pr()
  local mock_fetch = function()
    return 200, {}, cosmo.EncodeJson({{number = 42, title = "Test PR"}})
  end

  local pr_num = pr.find_pr_number("owner", "repo", "branch", "token", {fetch = mock_fetch})
  assert(pr_num == 42, "expected PR number 42")
end
test_finds_pr()

local function test_no_pr_found()
  local mock_fetch = function()
    return 200, {}, cosmo.EncodeJson({})
  end

  local pr_num, err = pr.find_pr_number("owner", "repo", "branch", "token", {fetch = mock_fetch})
  assert(not pr_num, "expected no PR number")
  assert(err and err:match("no open PR found"), "expected no open PR error")
end
test_no_pr_found()

local function test_update_pr_success()
  local captured_body
  local mock_fetch = function(_url, opts)
    captured_body = opts.body
    return 200, {}, cosmo.EncodeJson({number = 42, title = "New Title"})
  end

  local ok = pr.update_pr("owner", "repo", 42, "New Title", "New body", "token", {fetch = mock_fetch})
  assert(ok, "expected update to succeed")
  assert(captured_body:match("New Title"), "expected body to contain title")
  assert(captured_body:match("New body"), "expected body to contain body text")
end
test_update_pr_success()

local function test_update_pr_api_error()
  local mock_fetch = function()
    return 403, {}, cosmo.EncodeJson({message = "Forbidden"})
  end

  local ok, err = pr.update_pr("owner", "repo", 42, "Title", "Body", "token", {fetch = mock_fetch})
  assert(not ok, "expected update to fail")
  assert(err and err:match("403"), "expected 403 in error")
  assert(err and err:match("Forbidden"), "expected Forbidden in error")
end
test_update_pr_api_error()
