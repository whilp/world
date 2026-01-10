local unix = require("cosmo.unix")
local path = require("cosmo.path")
local cosmo = require("cosmo")

local function test_format_review_comment()
  package.loaded["lib.skill.pr-comments"] = nil
  local pr_comments = dofile("lib/skill/pr-comments.lua")

  local comment = {
    user = {login = "testuser"},
    path = "lib/foo.lua",
    line = 42,
    created_at = "2025-01-10T12:00:00Z",
    body = "This looks good!",
  }

  local output = pr_comments.format_review_comment(comment)
  assert(output:find("testuser"), "should include username")
  assert(output:find("lib/foo.lua"), "should include file path")
  assert(output:find("42"), "should include line number")
  assert(output:find("This looks good"), "should include body")
end

local function test_format_issue_comment()
  package.loaded["lib.skill.pr-comments"] = nil
  local pr_comments = dofile("lib/skill/pr-comments.lua")

  local comment = {
    user = {login = "reviewer"},
    created_at = "2025-01-10T12:00:00Z",
    body = "LGTM!",
  }

  local output = pr_comments.format_issue_comment(comment)
  assert(output:find("reviewer"), "should include username")
  assert(output:find("LGTM"), "should include body")
end

local function test_format_review()
  package.loaded["lib.skill.pr-comments"] = nil
  local pr_comments = dofile("lib/skill/pr-comments.lua")

  local review = {
    user = {login = "approver"},
    state = "APPROVED",
    submitted_at = "2025-01-10T12:00:00Z",
    body = "Ship it!",
  }

  local output = pr_comments.format_review(review)
  assert(output:find("approver"), "should include username")
  assert(output:find("APPROVED"), "should include state")
  assert(output:find("Ship it"), "should include body")
end

local function test_format_review_without_body()
  package.loaded["lib.skill.pr-comments"] = nil
  local pr_comments = dofile("lib/skill/pr-comments.lua")

  local review = {
    user = {login = "commenter"},
    state = "COMMENTED",
    submitted_at = "2025-01-10T12:00:00Z",
    body = "",
  }

  local output = pr_comments.format_review(review)
  assert(output:find("commenter"), "should include username")
  assert(output:find("COMMENTED"), "should include state")
end

local function test_reply_to_id()
  package.loaded["lib.skill.pr-comments"] = nil
  local pr_comments = dofile("lib/skill/pr-comments.lua")

  local comment = {
    user = {login = "replier"},
    path = "test.lua",
    line = 10,
    created_at = "2025-01-10T12:00:00Z",
    body = "I agree",
    in_reply_to_id = 12345,
  }

  local output = pr_comments.format_review_comment(comment)
  assert(output:find("12345"), "should include reply id")
end

local function test_parse_pr_url()
  package.loaded["lib.skill.pr-comments"] = nil
  local pr_comments = dofile("lib/skill/pr-comments.lua")

  local owner, repo, pr_num = pr_comments.parse_pr_url("https://github.com/whilp/world/pull/283")
  assert(owner == "whilp", "owner should be whilp")
  assert(repo == "world", "repo should be world")
  assert(pr_num == 283, "pr_num should be 283")

  -- test with trailing slash or query params
  owner, repo, pr_num = pr_comments.parse_pr_url("https://github.com/foo/bar/pull/42")
  assert(owner == "foo", "owner should be foo")
  assert(repo == "bar", "repo should be bar")
  assert(pr_num == 42, "pr_num should be 42")

  -- test invalid urls
  local invalid = pr_comments.parse_pr_url("https://gitlab.com/foo/bar/merge_requests/1")
  assert(invalid == nil, "gitlab url should return nil")
end

test_format_review_comment()
test_format_issue_comment()
test_format_review()
test_format_review_without_body()
test_reply_to_id()
test_parse_pr_url()

print("all tests passed")
