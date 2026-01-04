#!/usr/bin/env run-test.lua

local cosmic = require("skill.cosmic")
local cosmo = require("cosmo")

local function test_get_check_runs()
  local mock_fetch = function(url, opts)
    assert(url:match("/repos/owner/repo/commits/abc123/check%-runs"), "expected check-runs URL")
    assert(opts.method == "GET", "expected GET method")
    return 200, {}, cosmo.EncodeJson({
      check_runs = {
        {id = 1, name = "test", status = "completed", conclusion = "success"},
        {id = 2, name = "lint", status = "completed", conclusion = "failure"},
      }
    })
  end

  local pr = require("skill.pr")
  local runs = cosmic.get_check_runs("owner", "repo", "abc123", "token", {fetch = mock_fetch})
  assert(#runs == 2, "expected 2 check runs")
  assert(runs[1].name == "test", "expected first run to be 'test'")
  assert(runs[2].conclusion == "failure", "expected second run to fail")
end
test_get_check_runs()

local function test_get_pr_comments()
  local mock_fetch = function(url, opts)
    assert(url:match("/repos/owner/repo/issues/42/comments"), "expected comments URL")
    return 200, {}, cosmo.EncodeJson({
      {id = 1, body = "regular comment"},
      {id = 2, body = "<!-- cosmic-check-logs -->\n# Logs\nfoo"},
    })
  end

  local comments = cosmic.get_pr_comments("owner", "repo", 42, "token", {fetch = mock_fetch})
  assert(#comments == 2, "expected 2 comments")
end
test_get_pr_comments()

local function test_find_cosmic_comment()
  local comments = {
    {id = 1, body = "regular comment"},
    {id = 2, body = "<!-- cosmic-check-logs -->\n# Logs\nfoo"},
    {id = 3, body = "another comment"},
  }

  local found = cosmic.find_cosmic_comment(comments)
  assert(found, "expected to find cosmic comment")
  assert(found.id == 2, "expected comment id 2")
end
test_find_cosmic_comment()

local function test_find_cosmic_comment_not_found()
  local comments = {
    {id = 1, body = "regular comment"},
    {id = 2, body = "another comment"},
  }

  local found = cosmic.find_cosmic_comment(comments)
  assert(not found, "expected not to find cosmic comment")
end
test_find_cosmic_comment_not_found()

local function test_create_pr_comment()
  local captured_body
  local mock_fetch = function(url, opts)
    assert(url:match("/repos/owner/repo/issues/42/comments"), "expected comments URL")
    assert(opts.method == "POST", "expected POST method")
    captured_body = opts.body
    return 201, {}, cosmo.EncodeJson({id = 123})
  end

  local ok = cosmic.create_pr_comment("owner", "repo", 42, "test body", "token", {fetch = mock_fetch})
  assert(ok, "expected comment creation to succeed")
  assert(captured_body:match("test body"), "expected body to be captured")
end
test_create_pr_comment()

local function test_update_pr_comment()
  local captured_body
  local mock_fetch = function(url, opts)
    assert(url:match("/repos/owner/repo/issues/comments/123"), "expected comment URL")
    assert(opts.method == "PATCH", "expected PATCH method")
    captured_body = opts.body
    return 200, {}, cosmo.EncodeJson({id = 123})
  end

  local ok = cosmic.update_pr_comment("owner", "repo", 123, "updated body", "token", {fetch = mock_fetch})
  assert(ok, "expected comment update to succeed")
  assert(captured_body:match("updated body"), "expected body to be captured")
end
test_update_pr_comment()

local function test_create_comment_body()
  local body = cosmic.create_comment_body("test logs")
  assert(body:find("<!-- cosmic-check-logs -->", 1, true), "expected marker")
  assert(body:match("test logs"), "expected logs content")
  assert(body:match("Updated:"), "expected timestamp")
end
test_create_comment_body()

local function test_extract_logs_from_comment()
  local comment_body = "some prefix\n<!-- cosmic-check-logs -->\n# Logs\ntest content"
  local logs = cosmic.extract_logs_from_comment(comment_body)
  assert(logs, "expected logs to be extracted")
  assert(logs:find("cosmic-check-logs", 1, true), "expected marker in logs")
  assert(logs:match("test content"), "expected content in logs")
end
test_extract_logs_from_comment()

local function test_extract_logs_no_marker()
  local comment_body = "regular comment without marker"
  local logs = cosmic.extract_logs_from_comment(comment_body)
  assert(not logs, "expected no logs when marker not found")
end
test_extract_logs_no_marker()
