-- teal ignore: test file

local pr_comments = require("skill.pr_comments")

local record User
  login: string
end

local record ReviewComment
  id: number
  user: User
  path: string
  line: number
  created_at: string
  body: string
  in_reply_to_id: number
end

local record IssueComment
  user: User
  created_at: string
  body: string
end

local record Review
  user: User
  state: string
  submitted_at: string
  body: string
end

local record ParsedOpts
  owner: string
  repo: string
  pr_number: number
  json: boolean
end

local function test_format_review_comment()
  local comment: ReviewComment = {
    id = 12345,
    user = {login = "testuser"},
    path = "lib/foo.lua",
    line = 42,
    created_at = "2025-01-10T12:00:00Z",
    body = "This looks good!",
  }

  local output = pr_comments.format_review_comment(comment)
  assert(output:find("testuser"), "should include username")
  assert(output:find("12345"), "should include comment id")
  assert(output:find("lib/foo.lua"), "should include file path")
  assert(output:find("42"), "should include line number")
  assert(output:find("This looks good"), "should include body")
end
test_format_review_comment()

local function test_format_issue_comment()
  local comment: IssueComment = {
    user = {login = "reviewer"},
    created_at = "2025-01-10T12:00:00Z",
    body = "LGTM!",
  }

  local output = pr_comments.format_issue_comment(comment)
  assert(output:find("reviewer"), "should include username")
  assert(output:find("LGTM"), "should include body")
end
test_format_issue_comment()

local function test_format_review()
  local review: Review = {
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
test_format_review()

local function test_format_review_without_body()
  local review: Review = {
    user = {login = "commenter"},
    state = "COMMENTED",
    submitted_at = "2025-01-10T12:00:00Z",
    body = "",
  }

  local output = pr_comments.format_review(review)
  assert(output:find("commenter"), "should include username")
  assert(output:find("COMMENTED"), "should include state")
end
test_format_review_without_body()

local function test_reply_to_id()
  local comment: ReviewComment = {
    id = 99999,
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
test_reply_to_id()

local function test_parse_pr_url()
  local owner, repo, pr_num = pr_comments.parse_pr_url("https://github.com/whilp/world/pull/283")
  assert(owner == "whilp", "owner should be whilp")
  assert(repo == "world", "repo should be world")
  assert(pr_num == 283, "pr_num should be 283")

  owner, repo, pr_num = pr_comments.parse_pr_url("https://github.com/foo/bar/pull/42")
  assert(owner == "foo", "owner should be foo")
  assert(repo == "bar", "repo should be bar")
  assert(pr_num == 42, "pr_num should be 42")

  local invalid = pr_comments.parse_pr_url("https://gitlab.com/foo/bar/merge_requests/1")
  assert(invalid == nil, "gitlab url should return nil")
end
test_parse_pr_url()

local function test_parse_args()
  local opts = pr_comments.parse_args({"--owner", "whilp", "--repo", "world", "--pr", "283"}) as ParsedOpts
  assert(opts.owner == "whilp", "owner should be whilp")
  assert(opts.repo == "world", "repo should be world")
  assert(opts.pr_number == 283, "pr_number should be 283")
  assert(opts.json == false, "json should be false")

  opts = pr_comments.parse_args({"-o", "whilp", "-r", "world", "-p", "283", "--json"}) as ParsedOpts
  assert(opts.owner == "whilp", "owner should be whilp")
  assert(opts.json == true, "json should be true")

  opts = pr_comments.parse_args({"--url", "https://github.com/foo/bar/pull/42"}) as ParsedOpts
  assert(opts.owner == "foo", "owner should be foo")
  assert(opts.repo == "bar", "repo should be bar")
  assert(opts.pr_number == 42, "pr_number should be 42")
end
test_parse_args()
