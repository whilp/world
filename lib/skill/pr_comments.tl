-- Fetches review comments on a GitHub PR using cosmic.fetch
--
-- GitHub API endpoints:
--   /repos/{owner}/{repo}/pulls/{number}/comments - line-level review comments
--   /repos/{owner}/{repo}/issues/{number}/comments - general PR comments
--   /repos/{owner}/{repo}/pulls/{number}/reviews - reviews with their comments
--
-- Usage:
--   cosmic /zip/.lua/skill/pr_comments.lua --owner <owner> --repo <repo> --pr <number> [--json]
--   cosmic /zip/.lua/skill/pr_comments.lua --url <github_pr_url> [--json]

local cosmo = require("cosmo")
local getopt = require("cosmo.getopt")
local fetch = require("cosmic.fetch")

local record User
  login: string
end

local record ReviewComment
  id: integer
  user: User
  path: string
  line: integer
  original_line: integer
  created_at: string
  in_reply_to_id: integer
  body: string
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

local record ApiError
  message: string
end

local record ParsedOpts
  json: boolean
  owner: string
  repo: string
  pr_number: integer
  help: boolean
end

local record JsonResult
  reviews: {Review}
  issue_comments: {IssueComment}
  review_comments: {ReviewComment}
end

local record FetchResult
  ok: boolean
  status: number
  body: string
  error: string
end

local record FetchOpts
  method: string
  headers: {string:string}
  max_attempts: integer
  should_retry: function(FetchResult): boolean
end

local fetch_fn = fetch.Fetch as function(string, table): FetchResult

local function github_request(endpoint: string): integer, any
  local api_url = os.getenv("GITHUB_API_URL") or "https://api.github.com"
  local url = api_url .. endpoint

  local opts: FetchOpts = {
    method = "GET",
    headers = {
      ["Accept"] = "application/vnd.github+json",
      ["X-GitHub-Api-Version"] = "2022-11-28",
      ["User-Agent"] = "pr-comments.lua/1.0",
    },
    max_attempts = 4,
    should_retry = function(r: FetchResult): boolean return r.status >= 500 end,
  }

  local token = os.getenv("GITHUB_TOKEN")
  if token and token ~= "" then
    opts.headers["Authorization"] = "Bearer " .. token
  end

  local result = fetch_fn(url, opts as table)
  if not result.ok then
    return nil, result.error
  end

  local ok, decoded = pcall(cosmo.DecodeJson, result.body)
  if not ok then
    return nil, "json decode failed: " .. tostring(decoded)
  end

  if result.status ~= 200 then
    local api_err = decoded as ApiError
    local msg = api_err and api_err.message or "unknown error"
    return nil, string.format("api error %d: %s", result.status as integer, msg)
  end

  return result.status as integer, decoded
end

local function get_review_comments(owner: string, repo: string, pr_number: integer): integer, {ReviewComment}
  local endpoint = string.format("/repos/%s/%s/pulls/%d/comments", owner, repo, pr_number)
  return github_request(endpoint) as (integer, {ReviewComment})
end

local function get_issue_comments(owner: string, repo: string, pr_number: integer): integer, {IssueComment}
  local endpoint = string.format("/repos/%s/%s/issues/%d/comments", owner, repo, pr_number)
  return github_request(endpoint) as (integer, {IssueComment})
end

local function get_reviews(owner: string, repo: string, pr_number: integer): integer, {Review}
  local endpoint = string.format("/repos/%s/%s/pulls/%d/reviews", owner, repo, pr_number)
  return github_request(endpoint) as (integer, {Review})
end

local function format_review_comment(c: ReviewComment): string
  local lines: {string} = {}
  table.insert(lines, string.format("## Review comment by %s", c.user and c.user.login or "unknown"))
  table.insert(lines, string.format("ID: %s", c.id or "?"))
  table.insert(lines, string.format("File: %s (line %s)", c.path or "?", c.line or c.original_line or "?"))
  table.insert(lines, string.format("Created: %s", c.created_at or "?"))
  if c.in_reply_to_id then
    table.insert(lines, string.format("In reply to: %d", c.in_reply_to_id))
  end
  table.insert(lines, "")
  table.insert(lines, c.body or "")
  table.insert(lines, "")
  return table.concat(lines, "\n")
end

local function format_issue_comment(c: IssueComment): string
  local lines: {string} = {}
  table.insert(lines, string.format("## Comment by %s", c.user and c.user.login or "unknown"))
  table.insert(lines, string.format("Created: %s", c.created_at or "?"))
  table.insert(lines, "")
  table.insert(lines, c.body or "")
  table.insert(lines, "")
  return table.concat(lines, "\n")
end

local function format_review(r: Review): string
  local lines: {string} = {}
  local state = r.state or "UNKNOWN"
  table.insert(lines, string.format("## Review by %s [%s]", r.user and r.user.login or "unknown", state))
  table.insert(lines, string.format("Submitted: %s", r.submitted_at or "?"))
  if r.body and r.body ~= "" then
    table.insert(lines, "")
    table.insert(lines, r.body)
  end
  table.insert(lines, "")
  return table.concat(lines, "\n")
end

local function parse_pr_url(url: string): string, string, integer
  local owner, repo, pr_number = url:match("github%.com/([^/]+)/([^/]+)/pull/(%d+)")
  if owner and repo and pr_number then
    return owner, repo, tonumber(pr_number) as integer
  end
  return nil, nil, nil
end

local function parse_args(args: {string}): ParsedOpts
  local opts: ParsedOpts = {
    json = os.getenv("OUTPUT") == "json",
    owner = nil,
    repo = nil,
    pr_number = nil,
  }

  local longopts = {
    {"owner", "required", "o"},
    {"repo", "required", "r"},
    {"pr", "required", "p"},
    {"url", "required", "u"},
    {"json", "none", "j"},
    {"help", "none", "h"},
  }

  local parser = getopt.new(args, "o:r:p:u:jh", longopts)

  while true do
    local opt, optarg = parser:next()
    if not opt then break end

    if opt == "o" or opt == "owner" then
      opts.owner = optarg
    elseif opt == "r" or opt == "repo" then
      opts.repo = optarg
    elseif opt == "p" or opt == "pr" then
      opts.pr_number = tonumber(optarg) as integer
    elseif opt == "u" or opt == "url" then
      opts.owner, opts.repo, opts.pr_number = parse_pr_url(optarg)
    elseif opt == "j" or opt == "json" then
      opts.json = true
    elseif opt == "h" or opt == "help" then
      opts.help = true
    end
  end

  return opts
end

local function print_help()
  io.stderr:write([[
usage: cosmic /zip/.lua/skill/pr_comments.lua --owner <owner> --repo <repo> --pr <number> [--json]
       cosmic /zip/.lua/skill/pr_comments.lua --url <github_pr_url> [--json]

options:
  -o, --owner <owner>   repository owner
  -r, --repo <repo>     repository name
  -p, --pr <number>     pull request number
  -u, --url <url>       github pull request url
  -j, --json            output as json
  -h, --help            show this help

examples:
  cosmic /zip/.lua/skill/pr_comments.lua --owner whilp --repo world --pr 283
  cosmic /zip/.lua/skill/pr_comments.lua --url https://github.com/whilp/world/pull/283
  cosmic /zip/.lua/skill/pr_comments.lua -o whilp -r world -p 283 --json
]])
end

local function main(): integer
  local opts = parse_args(arg)

  if opts.help then
    print_help()
    return 0
  end

  if not opts.owner or not opts.repo or not opts.pr_number then
    io.stderr:write("error: missing required options\n")
    print_help()
    return 1
  end

  local status, review_comments = get_review_comments(opts.owner, opts.repo, opts.pr_number)
  if not status then
    io.stderr:write("error fetching review comments: " .. tostring(review_comments) .. "\n")
    return 1
  end

  local _, issue_comments = get_issue_comments(opts.owner, opts.repo, opts.pr_number)
  local _, reviews = get_reviews(opts.owner, opts.repo, opts.pr_number)

  if opts.json then
    local result: JsonResult = {
      reviews = reviews or {},
      issue_comments = issue_comments or {},
      review_comments = review_comments or {},
    }
    print(cosmo.EncodeJson(result))
    return 0
  end

  if reviews and #reviews > 0 then
    print("# Reviews\n")
    for _, r in ipairs(reviews) do
      print(format_review(r))
    end
  end

  if issue_comments and #issue_comments > 0 then
    print("# General Comments\n")
    for _, c in ipairs(issue_comments) do
      print(format_issue_comment(c))
    end
  end

  if review_comments and #review_comments > 0 then
    print("# Line Comments\n")
    for _, c in ipairs(review_comments) do
      print(format_review_comment(c))
    end
  end

  if (not reviews or #reviews == 0) and
     (not issue_comments or #issue_comments == 0) and
     (not review_comments or #review_comments == 0) then
    print("No comments found on PR #" .. opts.pr_number)
  end

  return 0
end

if cosmo.is_main() then
  os.exit(main() as integer)
end

local record Module
  record User
    login: string
  end

  record ReviewComment
    id: integer
    user: User
    path: string
    line: integer
    original_line: integer
    created_at: string
    in_reply_to_id: integer
    body: string
  end

  record IssueComment
    user: User
    created_at: string
    body: string
  end

  record Review
    user: User
    state: string
    submitted_at: string
    body: string
  end

  github_request: function(endpoint: string): integer, any
  get_review_comments: function(owner: string, repo: string, pr_number: integer): integer, {ReviewComment}
  get_issue_comments: function(owner: string, repo: string, pr_number: integer): integer, {IssueComment}
  get_reviews: function(owner: string, repo: string, pr_number: integer): integer, {Review}
  format_review_comment: function(c: ReviewComment): string
  format_issue_comment: function(c: IssueComment): string
  format_review: function(r: Review): string
  parse_pr_url: function(url: string): string, string, integer
  parse_args: function(args: {string}): ParsedOpts
  main: function(): integer
end

return {
  github_request = github_request,
  get_review_comments = get_review_comments,
  get_issue_comments = get_issue_comments,
  get_reviews = get_reviews,
  format_review_comment = format_review_comment,
  format_issue_comment = format_issue_comment,
  format_review = format_review,
  parse_pr_url = parse_pr_url,
  parse_args = parse_args,
  main = main,
} as Module
