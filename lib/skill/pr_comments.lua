-- teal ignore: type annotations needed
-- Fetches review comments on a GitHub PR using cosmo.Fetch
--
-- GitHub API endpoints:
--   /repos/{owner}/{repo}/pulls/{number}/comments - line-level review comments
--   /repos/{owner}/{repo}/issues/{number}/comments - general PR comments
--   /repos/{owner}/{repo}/pulls/{number}/reviews - reviews with their comments
--
-- Usage:
--   cosmic --skill pr_comments --owner <owner> --repo <repo> --pr <number> [--json]
--   cosmic --skill pr_comments --url <github_pr_url> [--json]

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local getopt = require("cosmo.getopt")

local function fetch_with_retry(url, opts, max_attempts)
  max_attempts = max_attempts or 4
  local status, headers, body
  local last_err

  for attempt = 1, max_attempts do
    status, headers, body = cosmo.Fetch(url, opts)
    if not status then
      last_err = "fetch failed: " .. tostring(headers)
    elseif status >= 500 then
      last_err = "server error: " .. tostring(status)
    else
      return status, headers, body
    end
    if attempt < max_attempts then
      unix.nanosleep(2 ^ attempt, 0)
    end
  end

  return nil, last_err
end

local function github_request(endpoint)
  local api_url = os.getenv("GITHUB_API_URL") or "https://api.github.com"
  local url = api_url .. endpoint

  local fetch_opts = {
    method = "GET",
    headers = {
      ["Accept"] = "application/vnd.github+json",
      ["X-GitHub-Api-Version"] = "2022-11-28",
      ["User-Agent"] = "pr-comments.lua/1.0",
    },
  }

  local token = os.getenv("GITHUB_TOKEN")
  if token and token ~= "" then
    fetch_opts.headers["Authorization"] = "Bearer " .. token
  end

  local status, headers, body = fetch_with_retry(url, fetch_opts)
  if not status then
    return nil, headers
  end

  local ok, decoded = pcall(cosmo.DecodeJson, body)
  if not ok then
    return nil, "json decode failed: " .. tostring(decoded)
  end

  if status ~= 200 then
    local msg = decoded and decoded.message or "unknown error"
    return nil, string.format("api error %d: %s", status, msg)
  end

  return status, decoded
end

local function get_review_comments(owner, repo, pr_number)
  local endpoint = string.format("/repos/%s/%s/pulls/%d/comments", owner, repo, pr_number)
  return github_request(endpoint)
end

local function get_issue_comments(owner, repo, pr_number)
  local endpoint = string.format("/repos/%s/%s/issues/%d/comments", owner, repo, pr_number)
  return github_request(endpoint)
end

local function get_reviews(owner, repo, pr_number)
  local endpoint = string.format("/repos/%s/%s/pulls/%d/reviews", owner, repo, pr_number)
  return github_request(endpoint)
end

local function format_review_comment(c)
  local lines = {}
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

local function format_issue_comment(c)
  local lines = {}
  table.insert(lines, string.format("## Comment by %s", c.user and c.user.login or "unknown"))
  table.insert(lines, string.format("Created: %s", c.created_at or "?"))
  table.insert(lines, "")
  table.insert(lines, c.body or "")
  table.insert(lines, "")
  return table.concat(lines, "\n")
end

local function format_review(r)
  local lines = {}
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

local function parse_pr_url(url)
  local owner, repo, pr_number = url:match("github%.com/([^/]+)/([^/]+)/pull/(%d+)")
  if owner and repo and pr_number then
    return owner, repo, tonumber(pr_number)
  end
  return nil
end

local function parse_args(args)
  local opts = {
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
      opts.pr_number = tonumber(optarg)
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
usage: cosmic --skill pr_comments --owner <owner> --repo <repo> --pr <number> [--json]
       cosmic --skill pr_comments --url <github_pr_url> [--json]

options:
  -o, --owner <owner>   repository owner
  -r, --repo <repo>     repository name
  -p, --pr <number>     pull request number
  -u, --url <url>       github pull request url
  -j, --json            output as json
  -h, --help            show this help

examples:
  cosmic --skill pr_comments --owner whilp --repo world --pr 283
  cosmic --skill pr_comments --url https://github.com/whilp/world/pull/283
  cosmic --skill pr_comments -o whilp -r world -p 283 --json
]])
end

local function main()
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
    local result = {
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
  os.exit(main())
end

return {
  fetch_with_retry = fetch_with_retry,
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
}
