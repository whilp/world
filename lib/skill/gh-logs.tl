--[[
Fetches GitHub Actions job info from:
  - Current branch (finds PR and latest check runs)
  - PR URL (finds latest check runs)
  - Action/job URL (shows that job)

Usage:
  cosmic --skill gh-logs                    # current branch
  cosmic --skill gh-logs <pr-url>           # PR's latest actions
  cosmic --skill gh-logs <action-url>       # specific job

Environment variables:
  GITHUB_TOKEN - optional, enables full log download with --logs
  GITHUB_API_URL - optional, defaults to https://api.github.com
]]

local cosmo = require("cosmo")
local getopt = require("cosmo.getopt")
local spawn_mod = require("cosmic.spawn")

local type SpawnHandle = require("cosmic.spawn").SpawnHandle

local record GetoptParser
  next: function(self): string, string
  remaining: function(self): {string}
  unknown: function(self): {string}
end

local record FetchOpts
  method: string
  headers: {string:string}
  body: string
  followredirect: boolean
  maxresponse: number
end

local type FetchFn = function(url: string, opts?: FetchOpts): number, {string:string}, string

local record Opts
  fetch: FetchFn
  getenv: function(string): string
  spawn: function({string}): SpawnHandle
end

local record ParsedUrl
  type: string
  owner: string
  repo: string
  run_id: string
  job_id: string
  pr_number: string
end

local record Step
  name: string
  number: number
  status: string
  conclusion: string
  started_at: string
  completed_at: string
end

local record Job
  id: number
  name: string
  status: string
  conclusion: string
  html_url: string
  started_at: string
  completed_at: string
  steps: {Step}
end

local record Annotation
  path: string
  start_line: number
  end_line: number
  annotation_level: string
  message: string
  title: string
end

local record CheckRun
  id: number
  name: string
  status: string
  conclusion: string
end

local record CheckRunsResponse
  total_count: number
  check_runs: {CheckRun}
end

local record JobsResponse
  total_count: number
  jobs: {Job}
end

local record PullRequest
  number: number
  title: string
  state: string
  html_url: string
  head: {sha: string}
end

local function log(msg: string)
  io.stderr:write("gh-logs: " .. msg .. "\n")
end

local function default_fetch(url: string, opts?: FetchOpts): number, {string:string}, string
  return cosmo.Fetch(url, opts as {string:any})
end

local function github_get(endpoint: string, opts?: Opts): any, number, {string:string}, string
  opts = opts or {}
  local fetch = opts.fetch or default_fetch
  local getenv = opts.getenv or os.getenv

  local api_url = getenv("GITHUB_API_URL") or "https://api.github.com"
  local url = api_url .. endpoint

  local headers: {string:string} = {
    ["Accept"] = "application/vnd.github+json",
    ["X-GitHub-Api-Version"] = "2022-11-28",
    ["User-Agent"] = "gh-logs/1.0",
  }

  local token = getenv("GITHUB_TOKEN")
  if token and token ~= "" then
    headers["Authorization"] = "Bearer " .. token
  end

  local status, resp_headers, body = fetch(url, {
    headers = headers,
    followredirect = false,
    maxresponse = 50 * 1024 * 1024,
  })

  if status == 200 then
    return cosmo.DecodeJson(body), nil, nil, nil
  end
  return nil, status, resp_headers, body
end

local function parse_url(url: string): ParsedUrl, string
  -- Job URL: github.com/owner/repo/actions/runs/run_id/job/job_id
  local owner, repo, run_id, job_id = url:match(
    "github%.com/([^/]+)/([^/]+)/actions/runs/(%d+)/job/(%d+)"
  )
  if owner then
    return { type = "job", owner = owner, repo = repo, run_id = run_id, job_id = job_id }
  end

  -- Run URL: github.com/owner/repo/actions/runs/run_id
  owner, repo, run_id = url:match(
    "github%.com/([^/]+)/([^/]+)/actions/runs/(%d+)"
  )
  if owner then
    return { type = "run", owner = owner, repo = repo, run_id = run_id }
  end

  -- PR URL: github.com/owner/repo/pull/number
  local pr_number: string
  owner, repo, pr_number = url:match(
    "github%.com/([^/]+)/([^/]+)/pull/(%d+)"
  )
  if owner then
    return { type = "pr", owner = owner, repo = repo, pr_number = pr_number }
  end

  return nil, "unrecognized URL format"
end

local function get_repo_info(opts?: Opts): string, string
  opts = opts or {}
  local spawn = opts.spawn or spawn_mod

  local h = spawn({"git", "remote", "get-url", "origin"})
  local ok, out = h:read()
  if not ok or not out then return nil, nil end

  local output = out as string
  -- Try github.com URLs
  local owner, repo = output:match("github%.com[:/]([^/]+)/([^/%.%s]+)")
  if not owner then
    -- Try local proxy format
    owner, repo = output:match("/git/([^/]+)/([^/%.%s]+)")
  end
  return owner, repo
end

local function get_current_branch(opts?: Opts): string
  opts = opts or {}
  local spawn = opts.spawn or spawn_mod

  local h = spawn({"git", "rev-parse", "--abbrev-ref", "HEAD"})
  local ok, out = h:read()
  if ok and out then
    return (out as string):match("^%s*(.-)%s*$")
  end
  return nil
end

local function get_head_sha(opts?: Opts): string
  opts = opts or {}
  local spawn = opts.spawn or spawn_mod

  local h = spawn({"git", "rev-parse", "HEAD"})
  local ok, out = h:read()
  if ok and out then
    return (out as string):match("^%s*(.-)%s*$")
  end
  return nil
end

local function find_pr_for_branch(owner: string, repo: string, branch: string, opts?: Opts): PullRequest
  local endpoint = string.format("/repos/%s/%s/pulls?head=%s:%s&state=all&per_page=1",
    owner, repo, owner, branch)
  local prs = github_get(endpoint, opts) as {PullRequest}
  if prs and #prs > 0 then
    return prs[1]
  end
  return nil
end

local function get_check_runs(owner: string, repo: string, sha: string, opts?: Opts): {CheckRun}
  local endpoint = string.format("/repos/%s/%s/commits/%s/check-runs", owner, repo, sha)
  local data = github_get(endpoint, opts) as CheckRunsResponse
  if data then
    return data.check_runs or {}
  end
  return {}
end

local function get_run_jobs(owner: string, repo: string, run_id: string, opts?: Opts): {Job}
  local endpoint = string.format("/repos/%s/%s/actions/runs/%s/jobs", owner, repo, run_id)
  local data = github_get(endpoint, opts) as JobsResponse
  if data then
    return data.jobs or {}
  end
  return {}
end

local function fetch_job(owner: string, repo: string, job_id: string, opts?: Opts): Job, string
  local endpoint = string.format("/repos/%s/%s/actions/jobs/%s", owner, repo, job_id)
  local data, status = github_get(endpoint, opts)
  if data then
    return data as Job, nil
  end
  return nil, string.format("failed to fetch job: %d", status or 0)
end

local function fetch_annotations(owner: string, repo: string, job_id: string, opts?: Opts): {Annotation}
  local endpoint = string.format("/repos/%s/%s/check-runs/%s/annotations", owner, repo, job_id)
  local data = github_get(endpoint, opts)
  if data then
    return data as {Annotation}
  end
  return {}
end

local function fetch_logs(owner: string, repo: string, job_id: string, opts?: Opts): string, string
  opts = opts or {}
  local fetch = opts.fetch or default_fetch
  local getenv = opts.getenv or os.getenv

  local token = getenv("GITHUB_TOKEN")
  if not token or token == "" then
    return nil, "GITHUB_TOKEN required for full logs"
  end

  local endpoint = string.format("/repos/%s/%s/actions/jobs/%s/logs", owner, repo, job_id)
  local _, status, headers, _ = github_get(endpoint, opts)

  if status == 302 and headers then
    local location = headers["location"]
    if location then
      local s, _, b = fetch(location, { maxresponse = 50 * 1024 * 1024 })
      if s == 200 then return b, nil end
    end
  end
  return nil, "failed to fetch logs"
end

local function format_duration(started: string, completed: string): string
  if not started or not completed then return "" end

  local function parse_ts(s: string): number
    local y, m, d, h, min, sec = s:match("(%d+)-(%d+)-(%d+)T(%d+):(%d+):(%d+)")
    if y then
      return os.time({ year = tonumber(y), month = tonumber(m), day = tonumber(d),
                       hour = tonumber(h), min = tonumber(min), sec = tonumber(sec) })
    end
    return nil
  end

  local t1, t2 = parse_ts(started), parse_ts(completed)
  if t1 and t2 then
    local diff = t2 - t1
    if diff < 60 then return string.format("%ds", diff) end
    return string.format("%dm%ds", math.floor(diff / 60), diff % 60)
  end
  return ""
end

local function print_job_summary(job: Job, annotations: {Annotation})
  local icon = job.conclusion == "success" and "✓" or
               job.conclusion == "failure" and "✗" or
               job.conclusion == "skipped" and "○" or "●"
  print(string.format("%s %s (%s)", icon, job.name, job.conclusion or job.status))
  print(string.format("  %s", job.html_url))

  if job.steps then
    for _, step in ipairs(job.steps) do
      local step_icon = step.conclusion == "success" and "✓" or
                        step.conclusion == "failure" and "✗" or
                        step.conclusion == "skipped" and "○" or "?"
      local duration = format_duration(step.started_at, step.completed_at)
      print(string.format("    %s %s %s", step_icon, step.name,
        duration ~= "" and "("..duration..")" or ""))
    end
  end

  if annotations and #annotations > 0 then
    for _, ann in ipairs(annotations) do
      if ann.annotation_level == "failure" or ann.annotation_level == "warning" then
        print(string.format("    [%s] %s", ann.annotation_level, ann.message))
      end
    end
  end
end

local function print_check_runs(check_runs: {CheckRun}, owner: string, repo: string, opts?: Opts)
  for _, cr in ipairs(check_runs) do
    local job = fetch_job(owner, repo, tostring(cr.id), opts)
    if job then
      local annotations = fetch_annotations(owner, repo, tostring(cr.id), opts)
      print_job_summary(job, annotations)
      print("")
    end
  end
end

local function print_help()
  print("usage: gh-logs [options] [url]")
  print("")
  print("Show GitHub Actions job status and logs.")
  print("")
  print("With no URL, finds the PR for the current branch.")
  print("With a PR URL, shows the latest check runs.")
  print("With an action URL, shows that specific job.")
  print("")
  print("options:")
  print("  -L, --logs     download full logs (requires GITHUB_TOKEN)")
  print("")
  print("examples:")
  print("  gh-logs")
  print("  gh-logs https://github.com/owner/repo/pull/123")
  print("  gh-logs https://github.com/owner/repo/actions/runs/123/job/456")
end

local function main(opts?: Opts): number, string
  opts = opts or {}
  local show_logs = false

  local parser = getopt.new(arg, "Lh", {
    {"logs", "none", "L"},
    {"help", "none", "h"},
  }) as GetoptParser

  while true do
    local opt = parser:next()
    if not opt then break end
    if opt == "L" or opt == "logs" then
      show_logs = true
    elseif opt == "h" or opt == "help" then
      print_help()
      return 0
    end
  end

  local remaining = parser:remaining()
  local url = remaining[1]

  local owner, repo = get_repo_info(opts)
  if not owner then
    return 1, "could not determine repository from git remote"
  end

  local parsed: ParsedUrl
  if url then
    local err: string
    parsed, err = parse_url(url)
    if not parsed then
      return 1, err
    end
    owner, repo = parsed.owner, parsed.repo
  end

  -- Handle different cases
  if not parsed then
    -- No URL: find PR for current branch
    local branch = get_current_branch(opts)
    if not branch then
      return 1, "could not determine current branch"
    end

    local pr = find_pr_for_branch(owner, repo, branch, opts)
    if pr then
      print(string.format("PR #%d: %s", pr.number, pr.title))
      print(string.format("  %s", pr.html_url))
      print("")
      local check_runs = get_check_runs(owner, repo, pr.head.sha, opts)
      if #check_runs == 0 then
        print("No check runs found")
      else
        print_check_runs(check_runs, owner, repo, opts)
      end
    else
      -- No PR, check runs for HEAD
      local sha = get_head_sha(opts)
      if sha then
        print(string.format("Branch: %s (%s)", branch, sha:sub(1,8)))
        print("")
        local check_runs = get_check_runs(owner, repo, sha, opts)
        if #check_runs == 0 then
          print("No check runs found for this commit")
        else
          print_check_runs(check_runs, owner, repo, opts)
        end
      end
    end

  elseif parsed.type == "pr" then
    -- PR URL: get PR info and check runs
    local pr_endpoint = string.format("/repos/%s/%s/pulls/%s", owner, repo, parsed.pr_number)
    local pr = github_get(pr_endpoint, opts) as PullRequest
    if not pr then
      return 1, "could not fetch PR"
    end
    print(string.format("PR #%d: %s", pr.number, pr.title))
    print(string.format("  %s", pr.html_url))
    print("")
    local check_runs = get_check_runs(owner, repo, pr.head.sha, opts)
    if #check_runs == 0 then
      print("No check runs found")
    else
      print_check_runs(check_runs, owner, repo, opts)
    end

  elseif parsed.type == "run" then
    -- Run URL: get all jobs for the run
    local jobs = get_run_jobs(owner, repo, parsed.run_id, opts)
    for _, job in ipairs(jobs) do
      local annotations = fetch_annotations(owner, repo, tostring(job.id), opts)
      print_job_summary(job, annotations)
      print("")
    end

  elseif parsed.type == "job" then
    -- Job URL: show single job
    local job, err = fetch_job(owner, repo, parsed.job_id, opts)
    if not job then
      return 1, "could not fetch job: " .. (err or "unknown")
    end

    if show_logs then
      local logs: string
      logs, err = fetch_logs(owner, repo, parsed.job_id, opts)
      if logs then
        io.write(logs)
      else
        log(err)
        local annotations = fetch_annotations(owner, repo, parsed.job_id, opts)
        print_job_summary(job, annotations)
        return 1
      end
    else
      local annotations = fetch_annotations(owner, repo, parsed.job_id, opts)
      print_job_summary(job, annotations)
    end
  end

  return 0
end

if cosmo.is_main() then
  local code, msg = main()
  if msg then
    log(msg)
  end
  os.exit(code)
end

local record GhLogsModule
  parse_url: function(url: string): ParsedUrl, string
  github_get: function(endpoint: string, opts?: Opts): any, number, {string:string}, string
  get_repo_info: function(opts?: Opts): string, string
  get_current_branch: function(opts?: Opts): string
  get_head_sha: function(opts?: Opts): string
  find_pr_for_branch: function(owner: string, repo: string, branch: string, opts?: Opts): PullRequest
  get_check_runs: function(owner: string, repo: string, sha: string, opts?: Opts): {CheckRun}
  get_run_jobs: function(owner: string, repo: string, run_id: string, opts?: Opts): {Job}
  fetch_job: function(owner: string, repo: string, job_id: string, opts?: Opts): Job, string
  fetch_annotations: function(owner: string, repo: string, job_id: string, opts?: Opts): {Annotation}
  fetch_logs: function(owner: string, repo: string, job_id: string, opts?: Opts): string, string
  main: function(opts?: Opts): number, string
end

return {
  parse_url = parse_url,
  github_get = github_get,
  get_repo_info = get_repo_info,
  get_current_branch = get_current_branch,
  get_head_sha = get_head_sha,
  find_pr_for_branch = find_pr_for_branch,
  get_check_runs = get_check_runs,
  get_run_jobs = get_run_jobs,
  fetch_job = fetch_job,
  fetch_annotations = fetch_annotations,
  fetch_logs = fetch_logs,
  main = main,
} as GhLogsModule
