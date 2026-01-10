-- Fetches review comments on a GitHub PR using cosmo.Fetch
--
-- GitHub API endpoints:
--   /repos/{owner}/{repo}/pulls/{number}/comments - line-level review comments
--   /repos/{owner}/{repo}/issues/{number}/comments - general PR comments
--   /repos/{owner}/{repo}/pulls/{number}/reviews - reviews with their comments
--
-- Usage:
--   pr-comments.lua <owner> <repo> <pr_number> [json]
--   pr-comments.lua https://github.com/owner/repo/pull/123 [json]
--   OUTPUT=json pr-comments.lua ...
--
-- Examples:
--   pr-comments.lua whilp world 283
--   pr-comments.lua https://github.com/whilp/world/pull/283 json
--   OUTPUT=json pr-comments.lua whilp world 283

local cosmo = require("cosmo")
local unix = require("cosmo.unix")

local function github_request(endpoint, opts)
  opts = opts or {}
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

  -- add auth header if token provided (proxy may handle this)
  local token = os.getenv("GITHUB_TOKEN")
  if token and token ~= "" then
    fetch_opts.headers["Authorization"] = "Bearer " .. token
  end

  -- retry with exponential backoff for transient errors
  local status, headers, body
  local last_err
  for attempt = 1, 4 do
    status, headers, body = cosmo.Fetch(url, fetch_opts)
    if not status then
      last_err = "fetch failed: " .. tostring(headers)
    elseif status >= 500 then
      last_err = "server error: " .. tostring(status)
    else
      break
    end
    if attempt < 4 then
      unix.nanosleep(2 ^ attempt, 0)
    end
  end

  if not status then
    return nil, last_err
  end

  if status >= 500 then
    return nil, last_err
  end

  local ok, decoded = pcall(cosmo.DecodeJson, body)
  if not ok then
    return nil, "json decode failed: " .. tostring(decoded)
  end

  -- check for api errors (rate limit, not found, etc)
  if status ~= 200 then
    local msg = decoded and decoded.message or "unknown error"
    return nil, string.format("api error %d: %s", status, msg)
  end

  return status, decoded
end

-- get line-level review comments (comments on specific lines of code)
local function get_review_comments(owner, repo, pr_number)
  local endpoint = string.format("/repos/%s/%s/pulls/%d/comments", owner, repo, pr_number)
  return github_request(endpoint)
end

-- get general pr/issue comments (not attached to specific lines)
local function get_issue_comments(owner, repo, pr_number)
  local endpoint = string.format("/repos/%s/%s/issues/%d/comments", owner, repo, pr_number)
  return github_request(endpoint)
end

-- get reviews (approval/request changes with optional body)
local function get_reviews(owner, repo, pr_number)
  local endpoint = string.format("/repos/%s/%s/pulls/%d/reviews", owner, repo, pr_number)
  return github_request(endpoint)
end

local function format_review_comment(c)
  local lines = {}
  table.insert(lines, string.format("## Review comment by %s", c.user and c.user.login or "unknown"))
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

-- parse github pr url: https://github.com/owner/repo/pull/123
local function parse_pr_url(url)
  local owner, repo, pr_number = url:match("github%.com/([^/]+)/([^/]+)/pull/(%d+)")
  if owner and repo and pr_number then
    return owner, repo, tonumber(pr_number)
  end
  return nil
end

local function print_help()
  io.stderr:write([[
usage: pr-comments.lua <owner> <repo> <pr_number> [json]
       pr-comments.lua <github_pr_url> [json]
       OUTPUT=json pr-comments.lua ...

examples:
  pr-comments.lua whilp world 283
  pr-comments.lua https://github.com/whilp/world/pull/283
  pr-comments.lua whilp world 283 json
  OUTPUT=json pr-comments.lua whilp world 283
]])
end

local function main(...)
  local args = {...}
  local owner, repo, pr_number
  local json_output = os.getenv("OUTPUT") == "json"

  -- check for json flag (without dash to avoid cosmic-lua eating it)
  for i, v in ipairs(args) do
    if v == "json" then
      json_output = true
      table.remove(args, i)
      break
    end
  end

  -- parse arguments: either URL or owner repo pr_number
  if #args == 1 then
    owner, repo, pr_number = parse_pr_url(args[1])
    if not owner then
      io.stderr:write("error: invalid github pr url\n")
      print_help()
      return 1
    end
  elseif #args == 3 then
    owner, repo = args[1], args[2]
    pr_number = tonumber(args[3])
    if not pr_number then
      io.stderr:write("error: pr_number must be a number\n")
      return 1
    end
  else
    print_help()
    return 1
  end

  -- fetch all comment types
  local status, review_comments = get_review_comments(owner, repo, pr_number)
  if not status then
    io.stderr:write("error fetching review comments: " .. tostring(review_comments) .. "\n")
    return 1
  end

  local _, issue_comments = get_issue_comments(owner, repo, pr_number)
  local _, reviews = get_reviews(owner, repo, pr_number)

  -- json output
  if json_output then
    local result = {
      reviews = reviews or {},
      issue_comments = issue_comments or {},
      review_comments = review_comments or {},
    }
    print(cosmo.EncodeJson(result))
    return 0
  end

  -- markdown output
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
    print("No comments found on PR #" .. pr_number)
  end

  return 0
end

if cosmo.is_main() then
  os.exit(main(table.unpack(arg)))
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
  main = main,
}
