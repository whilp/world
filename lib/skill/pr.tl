--[[
Updates a GitHub PR title and description from .github/pr/<name>.md

The PR file is identified by the x-cosmic-pr-name trailer in the commit message:

  cosmic: add feature

  Description of changes.

  x-cosmic-pr-name: feature-name.md

The pr.md format:
  # <title>

  <description>

Environment variables:
  GITHUB_TOKEN - required for API authentication
  GITHUB_REPOSITORY - owner/repo format
  GITHUB_PR_NUMBER - PR number (set by workflow)
  GITHUB_API_URL - optional, defaults to https://api.github.com
]]

local cosmo = require("cosmo")
local path = require("cosmo.path")
local spawn_mod = require("cosmic.spawn")

local type SpawnHandle = require("cosmic.spawn").SpawnHandle

local record FetchOpts
  method: string
  headers: {string:string}
  body: string
end

local type FetchFn = function(url: string, opts?: FetchOpts): number, {string:string} | string, string

local record SpawnOpts
  spawn: function({string}): SpawnHandle
  repo: string
end

local record RequestOpts
  fetch: FetchFn
  getenv: function(string): string
end

local record MainOpts
  spawn: function({string}): SpawnHandle
  repo: string
  getenv: function(string): string
  fetch: FetchFn
end

local record TrailerInfo
  commit_count: number
  first_sha: string
  last_sha: string
  winning_sha: string
  winning_trailer: string
end

local record ParsedPr
  title: string
  body: string
end

local record GitHubPr
  title: string
  body: string
  message: string
end

local function log(msg: string)
  io.stderr:write("pr: " .. msg .. "\n")
end

local function get_current_branch(): string
  local handle = spawn_mod({"git", "rev-parse", "--abbrev-ref", "HEAD"})
  local ok, out = handle:read()
  if not ok then
    return nil
  end
  local branch = (out as string):match("^%s*(.-)%s*$")
  if branch == "" then
    return nil
  end
  return branch
end

local function get_commit_sha(): string
  local handle = spawn_mod({"git", "rev-parse", "HEAD"})
  local ok, out = handle:read()
  if not ok or not out then
    return "unknown"
  end
  return (out as string):match("^%s*(.-)%s*$") or "unknown"
end

local function normalize_pr_name(name: string): string
  if not name then return nil end
  if not name:match("%.md$") then
    return name .. ".md"
  end
  return name
end

local function get_pr_files_from_branch(opts?: SpawnOpts): {string}
  opts = opts or {}
  local do_spawn = opts.spawn or spawn_mod

  -- Find files added on this branch matching .github/pr/*.md
  -- Compare against origin/HEAD (default branch)
  local cmd: {string} = {"git"}
  if opts.repo then
    table.insert(cmd, "-C")
    table.insert(cmd, opts.repo)
  end
  table.insert(cmd, "diff")
  table.insert(cmd, "--name-only")
  table.insert(cmd, "--diff-filter=A")
  table.insert(cmd, "origin/HEAD...HEAD")
  table.insert(cmd, "--")
  table.insert(cmd, ".github/pr/*.md")

  local handle = do_spawn(cmd)
  local ok, out = handle:read()
  if not ok or not out then
    return {}
  end

  local files: {string} = {}
  for line in (out as string):gmatch("[^\n]+") do
    local name = line:match("^%s*(.-)%s*$")
    if name and name ~= "" then
      table.insert(files, name)
    end
  end
  return files
end

local function get_pr_name_from_trailer(opts?: SpawnOpts): string, TrailerInfo
  opts = opts or {}
  local do_spawn = opts.spawn or spawn_mod

  -- Get all x-cosmic-pr-name and x-cosmic-pr-enable trailers from last 20 commits
  -- Process in chronological order (oldest first) to find the final state
  local cmd: {string} = {"git"}
  if opts.repo then
    table.insert(cmd, "-C")
    table.insert(cmd, opts.repo)
  end
  table.insert(cmd, "log")
  table.insert(cmd, "--format=%H %(trailers:key=x-cosmic-pr-name,valueonly)%(trailers:key=x-cosmic-pr-enable,valueonly)")
  table.insert(cmd, "--reverse")
  table.insert(cmd, "-20")
  local handle = do_spawn(cmd)
  local ok, out = handle:read()
  if not ok or not out then
    return nil, nil
  end

  -- Track the final state by processing commits in order
  local pr_name: string = nil
  local enabled = true
  local first_sha: string
  local last_sha: string
  local winning_sha: string
  local winning_trailer: string
  local commit_count = 0

  for line in (out as string):gmatch("[^\n]+") do
    local sha, rest = line:match("^(%S+)%s*(.*)$")
    if sha then
      commit_count = commit_count + 1
      if not first_sha then first_sha = sha end
      last_sha = sha

      local name = rest and rest:match("^%s*(.-)%s*$")
      if name and name ~= "" then
        if name == "false" then
          enabled = false
          pr_name = nil
          winning_sha = sha
          winning_trailer = "x-cosmic-pr-enable: false"
        else
          pr_name = name
          enabled = true
          winning_sha = sha
          winning_trailer = "x-cosmic-pr-name: " .. name
        end
      end
    end
  end

  local info: TrailerInfo = {
    commit_count = commit_count,
    first_sha = first_sha,
    last_sha = last_sha,
    winning_sha = winning_sha,
    winning_trailer = winning_trailer,
  }

  if enabled and pr_name then
    return pr_name, info
  end
  return nil, info
end


local function parse_pr_md(content: string): ParsedPr, string
  local title = content:match("^#%s*([^\n]+)")
  if not title then
    return nil, "no title found (expected '# <title>' on first line)"
  end

  title = title:match("^%s*(.-)%s*$")

  local body_start = content:find("\n", 1, true)
  local body = ""
  if body_start then
    body = content:sub(body_start + 1)
    body = body:match("^%s*(.-)%s*$") or ""
  end

  return {title = title, body = body}
end

local function default_fetch(url: string, opts?: FetchOpts): number, {string:string} | string, string
  return cosmo.Fetch(url, opts as {string:any})
end

local function github_request(method: string, endpoint: string, token: string, body: {string:string}, opts?: RequestOpts): number, GitHubPr | string
  opts = opts or {}
  local fetch = opts.fetch or default_fetch
  local getenv = opts.getenv or os.getenv

  local api_url = getenv("GITHUB_API_URL") or "https://api.github.com"
  local url = api_url .. endpoint

  local fetch_opts: FetchOpts = {
    method = method,
    headers = {
      ["Accept"] = "application/vnd.github+json",
      ["X-GitHub-Api-Version"] = "2022-11-28",
      ["User-Agent"] = "pr.lua/1.0",
    },
  }

  -- only add auth header if token provided
  if token and token ~= "" then
    fetch_opts.headers["Authorization"] = "Bearer " .. token
  end

  if body then
    fetch_opts.body = cosmo.EncodeJson(body)
    fetch_opts.headers["Content-Type"] = "application/json"
  end

  local status, headers, response_body = fetch(url, fetch_opts)
  if not status then
    return nil, "fetch failed: " .. tostring(headers)
  end

  local decoded_ok, decoded = pcall(cosmo.DecodeJson, response_body as string)
  if not decoded_ok then
    return nil, "json decode failed: " .. tostring(decoded)
  end

  return status, decoded as GitHubPr
end


local function get_pr(owner: string, repo: string, pr_number: number, token: string, opts?: RequestOpts): GitHubPr, string
  local endpoint = string.format("/repos/%s/%s/pulls/%d", owner, repo, pr_number)

  local status, data = github_request("GET", endpoint, token, nil, opts)
  if not status then
    return nil, data as string
  end

  if status ~= 200 then
    local pr_data = data as GitHubPr
    local msg = pr_data and pr_data.message or "unknown error"
    return nil, "api error " .. tostring(status) .. ": " .. msg
  end

  return data as GitHubPr
end

local function append_timestamp_details(body: string): string
  local timestamp = os.date("!%Y-%m-%dT%H:%M:%SZ")
  local entry = string.format("- Updated: %s", timestamp)

  local details_marker = "<!-- pr-update-history -->"
  local details_block = details_marker .. "\n" ..
                       "<details><summary>Update history</summary>\n\n" ..
                       entry .. "\n</details>"

  -- check if details section already exists
  local marker_pos = body:find(details_marker, 1, true)
  if marker_pos then
    -- find the end of the details section
    local details_close = body:find("</details>", marker_pos, true)
    if details_close then
      -- replace the entire details block
      local before = body:sub(1, marker_pos - 1)
      local after = body:sub(details_close + #"</details>")
      -- trim trailing newline from before if present
      before = before:match("^(.-)%s*$")
      return before .. "\n\n" .. details_block .. after
    end
  end

  -- no existing details section, append new one
  local separator = body:match("\n$") and "" or "\n"
  return body .. separator .. "\n" .. details_block
end

local function update_pr(owner: string, repo: string, pr_number: number, title: string, body: string, token: string, opts?: RequestOpts): boolean, string
  local endpoint = string.format("/repos/%s/%s/pulls/%d", owner, repo, pr_number)

  local status, data = github_request("PATCH", endpoint, token, {
    title = title,
    body = body,
  }, opts)

  if not status then
    return nil, data as string
  end

  if status ~= 200 then
    local pr_data = data as GitHubPr
    local msg = pr_data and pr_data.message or "unknown error"
    return nil, "api error " .. tostring(status) .. ": " .. msg
  end

  return true
end


local function print_help()
  local pr_name = get_pr_name_from_trailer()
  local pr_file: string
  if pr_name then
    pr_file = path.join(".github/pr", normalize_pr_name(pr_name))
  else
    pr_file = ".github/pr/<name>.md"
  end

  local help = string.format([[
usage: pr.lua [-h]

Updates PR title and description from %s

The PR file is identified by the x-cosmic-pr-name trailer in your commit message:

    cosmic: add feature

    Description of changes.

    x-cosmic-pr-name: feature-name.md

The file format:

    # component: verb explanation

    Brief description of changes.

    - file1.lua - what it does
    - file2.lua - what it does

    ## Validation

    - [x] tests pass
    - [x] linter passes

Guidelines:

  1. Choose a descriptive name for your PR file (e.g., feature-name.md)
  2. Create %s
  3. Add x-cosmic-pr-name: <name>.md trailer to your commit message
  4. Follow repo conventions: `# component: verb explanation` title
  5. Keep content concise but include key decisions, tradeoffs, examples
  6. Update the file as the PR evolves
  7. Push to trigger the workflow and update the PR

Environment variables (set automatically in GitHub Actions):
  GITHUB_TOKEN       - required for API authentication
  GITHUB_REPOSITORY  - owner/repo format
  GITHUB_PR_NUMBER   - PR number
]], pr_file, pr_file)

  if pr_name then
    help = help .. string.format("\nDetected PR file: %s\n", pr_file)
  end

  print(help)
end

local function is_github_actions(): boolean
  return os.getenv("GITHUB_ACTIONS") == "true"
end

local function do_update(owner: string, repo_name: string, pr_number: number, pr_name: string, trailer_info: TrailerInfo, token: string, opts?: RequestOpts): number, string
  -- log commit range and winning trailer
  if trailer_info and trailer_info.first_sha then
    local range = string.format("%s..%s (%d commits)",
      trailer_info.first_sha:sub(1, 8),
      trailer_info.last_sha:sub(1, 8),
      trailer_info.commit_count)
    log("checked " .. range)
  end
  if trailer_info and trailer_info.winning_sha then
    log("using " .. trailer_info.winning_sha:sub(1, 8) .. " " .. trailer_info.winning_trailer)
  end

  local pr_file = path.join(".github/pr", normalize_pr_name(pr_name))

  if not path.exists(pr_file) then
    log(pr_file .. " not found, skipping")
    return 0
  end

  local content = cosmo.Slurp(pr_file)
  if not content then
    return 1, "failed to read " .. pr_file
  end

  local pr, err = parse_pr_md(content)
  if not pr then
    return 1, "failed to parse " .. pr_file .. ": " .. err
  end

  -- get current PR state to check if we're making changes
  local current_pr: GitHubPr
  current_pr, err = get_pr(owner, repo_name, pr_number, token, opts)
  if not current_pr then
    return 1, "failed to get current PR: " .. err
  end

  -- check if title or body has changed
  local title_changed = current_pr.title ~= pr.title
  local body_changed = current_pr.body ~= pr.body

  local body_to_update = pr.body
  if title_changed or body_changed then
    -- append timestamp details when making actual changes
    body_to_update = append_timestamp_details(pr.body)
  end

  local ok: boolean
  ok, err = update_pr(owner, repo_name, pr_number, pr.title, body_to_update, token, opts)
  if not ok then
    return 1, "failed to update PR: " .. err
  end

  log("updated PR #" .. pr_number .. ": " .. pr.title)
  return 0
end

local function main(opts?: MainOpts): number, string
  opts = opts or {}
  local getenv = opts.getenv or os.getenv

  local is_actions = getenv("GITHUB_ACTIONS") == "true"
  if not is_actions then
    print_help()
    return 0
  end

  local token = getenv("GITHUB_TOKEN")
  if not token or token == "" then
    return 1, "GITHUB_TOKEN not set"
  end

  local repo = getenv("GITHUB_REPOSITORY")
  if not repo then
    return 1, "GITHUB_REPOSITORY not set"
  end

  local owner, repo_name = repo:match("^([^/]+)/(.+)$")
  if not owner then
    return 1, "invalid GITHUB_REPOSITORY format: " .. repo
  end

  local pr_number_str = getenv("GITHUB_PR_NUMBER")
  if not pr_number_str or pr_number_str == "" then
    return 1, "GITHUB_PR_NUMBER not set"
  end
  local pr_number = tonumber(pr_number_str)
  if not pr_number then
    return 1, "invalid GITHUB_PR_NUMBER"
  end

  local spawn_opts: SpawnOpts = {spawn = opts.spawn, repo = opts.repo}
  local pr_name, trailer_info = get_pr_name_from_trailer(spawn_opts)
  if not pr_name then
    -- fallback: check if branch introduces exactly one .github/pr/*.md file
    local pr_files = get_pr_files_from_branch(spawn_opts)
    if #pr_files == 1 then
      -- extract just the filename from .github/pr/<name>.md
      local basename = pr_files[1]:match("%.github/pr/(.+)$")
      if basename then
        log("auto-detected PR file from branch: " .. pr_files[1])
        pr_name = basename
      end
    end
  end

  if not pr_name then
    local first = trailer_info and trailer_info.first_sha and trailer_info.first_sha:sub(1, 8) or "unknown"
    local last = trailer_info and trailer_info.last_sha and trailer_info.last_sha:sub(1, 8) or "unknown"

    local msg = string.format(
      "no x-cosmic-pr-name trailer found (or disabled) in last 20 commits (%s .. %s)\n\nNote: x-cosmic-pr-enable: false disables updates until a new x-cosmic-pr-name is set",
      first,
      last
    )
    return 0, msg
  end

  local request_opts: RequestOpts = {fetch = opts.fetch, getenv = opts.getenv}
  return do_update(owner, repo_name, pr_number, pr_name, trailer_info, token, request_opts)
end

if cosmo.is_main() then
  if arg[1] == "-h" or arg[1] == "--help" then
    print_help()
    os.exit(0)
  end
  local _, msg = main()
  if msg then
    log(msg)
  end
  os.exit(0)
end

local record PrModule
  parse_pr_md: function(content: string): ParsedPr, string
  github_request: function(method: string, endpoint: string, token: string, body: {string:string}, opts?: RequestOpts): number, GitHubPr
  get_pr: function(owner: string, repo: string, pr_number: number, token: string, opts?: RequestOpts): GitHubPr, string
  append_timestamp_details: function(body: string): string
  update_pr: function(owner: string, repo: string, pr_number: number, title: string, body: string, token: string, opts?: RequestOpts): boolean, string
  get_current_branch: function(): string
  get_commit_sha: function(): string
  get_pr_name_from_trailer: function(opts?: SpawnOpts): string, TrailerInfo
  get_pr_files_from_branch: function(opts?: SpawnOpts): {string}
  is_github_actions: function(): boolean
  do_update: function(owner: string, repo_name: string, pr_number: number, pr_name: string, trailer_info: TrailerInfo, token: string, opts?: RequestOpts): number, string
  main: function(opts?: MainOpts): number, string
end

return {
  parse_pr_md = parse_pr_md,
  github_request = github_request,
  get_pr = get_pr,
  append_timestamp_details = append_timestamp_details,
  update_pr = update_pr,
  get_current_branch = get_current_branch,
  get_commit_sha = get_commit_sha,
  get_pr_name_from_trailer = get_pr_name_from_trailer,
  get_pr_files_from_branch = get_pr_files_from_branch,
  is_github_actions = is_github_actions,
  do_update = do_update,
  main = main,
} as PrModule
