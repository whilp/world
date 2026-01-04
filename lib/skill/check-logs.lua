-- teal ignore: type annotations needed
--[[
Manages GitHub check run logs in PR comments

GitHub Actions mode (GITHUB_ACTIONS=true):
  - Reads check run logs from the PR
  - Creates or updates a PR comment with logs in <details> tags

Local mode:
  - Finds the PR for the current branch
  - Reads check logs from the cosmic comment on the PR

Environment variables:
  GITHUB_TOKEN - required for API authentication
  GITHUB_REPOSITORY - owner/repo format
  GITHUB_PR_NUMBER - PR number (set by workflow)
  GITHUB_HEAD_REF - branch name (fallback to find PR)
  GITHUB_API_URL - optional, defaults to https://api.github.com
  GITHUB_SHA - commit SHA for check runs
]]

local cosmo = require("cosmo")
local pr = require("skill.pr")

local function log(msg)
  io.stderr:write("check-logs: " .. msg .. "\n")
end

local COMMENT_MARKER = "<!-- check-logs -->"

local function get_check_runs(owner, repo, ref, token, opts)
  local endpoint = string.format("/repos/%s/%s/commits/%s/check-runs", owner, repo, ref)

  local status, data = pr.github_request("GET", endpoint, token, nil, opts)
  if not status then
    return nil, data
  end

  if status ~= 200 then
    local msg = data and data.message or "unknown error"
    return nil, "api error " .. tostring(status) .. ": " .. msg
  end

  return data.check_runs or {}
end

local function get_check_run_logs(owner, repo, run_id, token, opts)
  opts = opts or {}
  local fetch = opts.fetch or function(url, fetch_opts)
    return cosmo.Fetch(url, fetch_opts)
  end
  local getenv = opts.getenv or os.getenv

  local api_url = getenv("GITHUB_API_URL") or "https://api.github.com"
  local url = string.format("%s/repos/%s/%s/actions/runs/%d/logs",
    api_url, owner, repo, run_id)

  local fetch_opts = {
    method = "GET",
    headers = {
      ["Accept"] = "application/vnd.github+json",
      ["X-GitHub-Api-Version"] = "2022-11-28",
      ["User-Agent"] = "check-logs.lua/1.0",
    },
  }

  if token and token ~= "" then
    fetch_opts.headers["Authorization"] = "Bearer " .. token
  end

  local status, _, body = fetch(url, fetch_opts)
  if not status or status ~= 200 then
    return nil
  end

  return body
end

local function format_check_logs(check_runs, owner, repo, token, opts)
  opts = opts or {}
  local get_logs = opts.get_logs or get_check_run_logs
  local sections = {}

  for _, run in ipairs(check_runs) do
    if run.status == "completed" and run.conclusion ~= "success" then
      local logs = get_logs(owner, repo, run.id, token, opts)

      local conclusion = run.conclusion or "unknown"
      local summary = string.format("**%s** (%s)", run.name, conclusion)

      if logs and logs ~= "" then
        local details = string.format(
          "<details>\n<summary>%s</summary>\n\n```\n%s\n```\n</details>",
          summary, logs
        )
        table.insert(sections, details)
      else
        table.insert(sections, summary .. " (no logs available)")
      end
    end
  end

  if #sections == 0 then
    return nil
  end

  return table.concat(sections, "\n\n")
end

local function get_pr_comments(owner, repo, pr_number, token, opts)
  local endpoint = string.format("/repos/%s/%s/issues/%d/comments", owner, repo, pr_number)

  local status, data = pr.github_request("GET", endpoint, token, nil, opts)
  if not status then
    return nil, data
  end

  if status ~= 200 then
    local msg = data and data.message or "unknown error"
    return nil, "api error " .. tostring(status) .. ": " .. msg
  end

  return data
end

local function find_cosmic_comment(comments)
  if not comments or type(comments) ~= "table" then
    return nil
  end

  for _, comment in ipairs(comments) do
    if comment.body and comment.body:find(COMMENT_MARKER, 1, true) then
      return comment
    end
  end

  return nil
end

local function create_pr_comment(owner, repo, pr_number, body, token, opts)
  local endpoint = string.format("/repos/%s/%s/issues/%d/comments", owner, repo, pr_number)

  local status, data = pr.github_request("POST", endpoint, token, {body = body}, opts)
  if not status then
    return nil, data
  end

  if status ~= 201 then
    local msg = data and data.message or "unknown error"
    return nil, "api error " .. tostring(status) .. ": " .. msg
  end

  return data
end

local function update_pr_comment(owner, repo, comment_id, body, token, opts)
  local endpoint = string.format("/repos/%s/%s/issues/comments/%d", owner, repo, comment_id)

  local status, data = pr.github_request("PATCH", endpoint, token, {body = body}, opts)
  if not status then
    return nil, data
  end

  if status ~= 200 then
    local msg = data and data.message or "unknown error"
    return nil, "api error " .. tostring(status) .. ": " .. msg
  end

  return data
end

local function create_comment_body(formatted_logs)
  local timestamp = os.date("!%Y-%m-%dT%H:%M:%SZ")
  return COMMENT_MARKER .. "\n\n" ..
         "# Check run logs\n\n" ..
         "Updated: " .. timestamp .. "\n\n" ..
         formatted_logs
end

local function extract_logs_from_comment(comment_body)
  if not comment_body then
    return nil
  end

  -- find the marker
  local marker_pos = comment_body:find(COMMENT_MARKER, 1, true)
  if not marker_pos then
    return nil
  end

  -- return everything after the marker
  return comment_body:sub(marker_pos)
end

local function do_github_actions_mode(owner, repo_name, pr_number, sha, token, opts)
  log(string.format("fetching check runs for PR #%d at %s", pr_number, sha:sub(1, 7)))

  local check_runs, err = get_check_runs(owner, repo_name, sha, token, opts)
  if not check_runs then
    return 1, "failed to get check runs: " .. err
  end

  log(string.format("found %d check runs", #check_runs))

  local formatted_logs = format_check_logs(check_runs, owner, repo_name, token, opts)
  if not formatted_logs then
    log("no failed checks with logs to report")
    return 0
  end

  local comments
  comments, err = get_pr_comments(owner, repo_name, pr_number, token, opts)
  if not comments then
    return 1, "failed to get PR comments: " .. err
  end

  local existing_comment = find_cosmic_comment(comments)
  local comment_body = create_comment_body(formatted_logs)

  if existing_comment then
    log(string.format("updating existing comment #%d", existing_comment.id))
    local ok
    ok, err = update_pr_comment(owner, repo_name, existing_comment.id, comment_body, token, opts)
    if not ok then
      return 1, "failed to update comment: " .. err
    end
  else
    log("creating new comment")
    local ok
    ok, err = create_pr_comment(owner, repo_name, pr_number, comment_body, token, opts)
    if not ok then
      return 1, "failed to create comment: " .. err
    end
  end

  log("check logs updated on PR #" .. pr_number)
  return 0
end

local function do_local_mode(owner, repo_name, pr_number, token, opts)
  log(string.format("fetching check logs for PR #%d", pr_number))

  local comments, err = get_pr_comments(owner, repo_name, pr_number, token, opts)
  if not comments then
    return 1, "failed to get PR comments: " .. err
  end

  local existing_comment = find_cosmic_comment(comments)
  if not existing_comment then
    return 1, "no check logs comment found on PR #" .. pr_number
  end

  local logs = extract_logs_from_comment(existing_comment.body)
  if not logs then
    return 1, "failed to extract logs from comment"
  end

  print(logs)
  return 0
end

local function main(opts)
  opts = opts or {}
  local getenv = opts.getenv or os.getenv

  local is_actions = getenv("GITHUB_ACTIONS") == "true"

  local token = getenv("GITHUB_TOKEN")
  if not is_actions and (not token or token == "") then
    log("note: GITHUB_TOKEN not set, using unauthenticated requests (may fail for private repos)")
  end

  local repo = getenv("GITHUB_REPOSITORY")
  local owner, repo_name

  if repo then
    owner, repo_name = repo:match("^([^/]+)/(.+)$")
    if not owner then
      return 1, "invalid GITHUB_REPOSITORY format: " .. repo
    end
  else
    if is_actions then
      return 1, "GITHUB_REPOSITORY not set"
    end

    -- try to get from git
    local git_info = pr.get_git_info()
    if not git_info then
      return 1, "could not determine repository (GITHUB_REPOSITORY not set and not in a git repo)"
    end

    owner = git_info.owner
    repo_name = git_info.repo
  end

  local pr_number, err = pr.get_pr_number_from_env(opts)
  if not pr_number then
    return 1, "could not determine PR number: " .. err
  end

  if is_actions then
    if not token or token == "" then
      return 1, "GITHUB_TOKEN not set"
    end

    local sha = getenv("GITHUB_SHA")
    if not sha or sha == "" then
      return 1, "GITHUB_SHA not set"
    end

    return do_github_actions_mode(owner, repo_name, pr_number, sha, token, opts)
  else
    return do_local_mode(owner, repo_name, pr_number, token, opts)
  end
end

if cosmo.is_main() then
  local code, msg = main()
  if msg then
    log(msg)
  end
  os.exit(code or 0)
end

return {
  get_check_runs = get_check_runs,
  get_check_run_logs = get_check_run_logs,
  format_check_logs = format_check_logs,
  get_pr_comments = get_pr_comments,
  find_cosmic_comment = find_cosmic_comment,
  create_pr_comment = create_pr_comment,
  update_pr_comment = update_pr_comment,
  create_comment_body = create_comment_body,
  extract_logs_from_comment = extract_logs_from_comment,
  main = main,
}
