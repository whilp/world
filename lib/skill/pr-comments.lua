-- pr-comments - fetch review comments for a PR
-- usage: cosmic -e "require('skill.pr-comments').main()"

local cosmo = require("cosmo")

local function github_request(method, endpoint, token)
  local api_url = os.getenv("GITHUB_API_URL") or "https://api.github.com"
  local url = api_url .. endpoint

  local fetch_opts = {
    method = method,
    headers = {
      ["Accept"] = "application/vnd.github+json",
      ["X-GitHub-Api-Version"] = "2022-11-28",
      ["User-Agent"] = "pr-comments.lua/1.0",
    },
  }

  if token and token ~= "" then
    fetch_opts.headers["Authorization"] = "Bearer " .. token
  end

  local status, headers, response_body = cosmo.Fetch(url, fetch_opts)
  if not status then
    return nil, "fetch failed: " .. tostring(headers)
  end

  local ok, decoded = pcall(cosmo.DecodeJson, response_body)
  if not ok then
    return nil, "json decode failed: " .. tostring(decoded)
  end

  return status, decoded
end

-- find PR by branch name
local function find_pr_by_branch(owner, repo, branch, token)
  local endpoint = string.format("/repos/%s/%s/pulls?head=%s:%s&state=open",
    owner, repo, owner, branch)

  local status, data = github_request("GET", endpoint, token)
  if not status then
    return nil, data
  end

  if status ~= 200 then
    return nil, "api error: " .. tostring(status)
  end

  if #data == 0 then
    return nil, "no open PR found for branch: " .. branch
  end

  return data[1]
end

-- get review comments (comments on specific lines of code)
local function get_review_comments(owner, repo, pr_number, token)
  local endpoint = string.format("/repos/%s/%s/pulls/%d/comments", owner, repo, pr_number)

  local status, data = github_request("GET", endpoint, token)
  if not status then
    return nil, data
  end

  if status ~= 200 then
    return nil, "api error: " .. tostring(status)
  end

  return data
end

-- get issue comments (general PR comments, not on specific lines)
local function get_issue_comments(owner, repo, pr_number, token)
  local endpoint = string.format("/repos/%s/%s/issues/%d/comments", owner, repo, pr_number)

  local status, data = github_request("GET", endpoint, token)
  if not status then
    return nil, data
  end

  if status ~= 200 then
    return nil, "api error: " .. tostring(status)
  end

  return data
end

-- get reviews (approval/request changes with body)
local function get_reviews(owner, repo, pr_number, token)
  local endpoint = string.format("/repos/%s/%s/pulls/%d/reviews", owner, repo, pr_number)

  local status, data = github_request("GET", endpoint, token)
  if not status then
    return nil, data
  end

  if status ~= 200 then
    return nil, "api error: " .. tostring(status)
  end

  return data
end

local function format_comment(c, kind)
  local author = c.user and c.user.login or "unknown"
  local body = c.body or ""
  local created = c.created_at or ""

  local header = string.format("## %s by @%s (%s)", kind, author, created:sub(1, 10))

  if c.path then
    header = header .. string.format("\nFile: %s", c.path)
    if c.line then
      header = header .. string.format(":%d", c.line)
    elseif c.original_line then
      header = header .. string.format(":%d", c.original_line)
    end
  end

  if c.state and c.state ~= "COMMENTED" then
    header = header .. string.format("\nState: %s", c.state)
  end

  return header .. "\n\n" .. body .. "\n"
end

local function main()
  local token = os.getenv("GITHUB_TOKEN")
  if not token or token == "" then
    io.stderr:write("GITHUB_TOKEN not set\n")
    io.stderr:write("Set it with: export GITHUB_TOKEN=<your-token>\n")
    return 1
  end

  local repo = os.getenv("GITHUB_REPOSITORY") or "whilp/world"
  local owner, repo_name = repo:match("^([^/]+)/(.+)$")
  if not owner then
    io.stderr:write("invalid GITHUB_REPOSITORY: " .. repo .. "\n")
    return 1
  end

  -- get branch from current git branch
  local spawn = require("cosmic.spawn").spawn
  local handle = spawn({"git", "rev-parse", "--abbrev-ref", "HEAD"})
  local ok, out = handle:read()
  local branch
  if ok and out then
    branch = out:match("^%s*(.-)%s*$")
  end

  if not branch then
    io.stderr:write("could not determine branch\n")
    return 1
  end

  io.stderr:write("Looking for PR on branch: " .. branch .. "\n")

  -- find PR
  local pr, err = find_pr_by_branch(owner, repo_name, branch, token)
  if not pr then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    return 1
  end

  local pr_number = pr.number
  io.stderr:write(string.format("Found PR #%d: %s\n\n", pr_number, pr.title))

  -- get all types of comments
  local review_comments = get_review_comments(owner, repo_name, pr_number, token) or {}
  local issue_comments = get_issue_comments(owner, repo_name, pr_number, token) or {}
  local reviews = get_reviews(owner, repo_name, pr_number, token) or {}

  local has_comments = false

  -- print reviews with body
  for _, r in ipairs(reviews) do
    if r.body and r.body ~= "" then
      print(format_comment(r, "Review"))
      has_comments = true
    end
  end

  -- print issue comments
  for _, c in ipairs(issue_comments) do
    print(format_comment(c, "Comment"))
    has_comments = true
  end

  -- print review comments (on specific lines)
  for _, c in ipairs(review_comments) do
    print(format_comment(c, "Line Comment"))
    has_comments = true
  end

  if not has_comments then
    print("No comments found on this PR.")
  end

  return 0
end

if cosmo.is_main() then
  os.exit(main())
end

return {
  main = main,
  find_pr_by_branch = find_pr_by_branch,
  get_review_comments = get_review_comments,
  get_issue_comments = get_issue_comments,
  get_reviews = get_reviews,
}
