--[[
Updates a GitHub PR title and description from .github/pr/<name>.md

The PR file is auto-detected: if the branch adds exactly one .github/pr/*.md
file, that file is used to update the PR title and description.

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
local spawn_mod = require("cosmic.child")
local fetch_mod = require("cosmic.fetch")

local type Handle = require("cosmic.child").Handle

local record FetchOpts
  method: string
  headers: {string:string}
  body: string
end

local type FetchResult = require("cosmic.fetch").Result
local type FetchFn = function(url: string, opts?: FetchOpts): FetchResult

local record SpawnOpts
  spawn: function({string}): Handle
  repo: string
  getenv: function(string): string
end

local record RequestOpts
  fetch: FetchFn
  getenv: function(string): string
end

local record MainOpts
  spawn: function({string}): Handle
  repo: string
  getenv: function(string): string
  fetch: FetchFn
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

local function get_default_branch(opts?: SpawnOpts): string
  opts = opts or {}
  local getenv = opts.getenv or os.getenv

  -- In GitHub Actions PR context, GITHUB_BASE_REF is always set
  local base_ref = getenv("GITHUB_BASE_REF")
  if base_ref and base_ref ~= "" then
    return "origin/" .. base_ref
  end

  return "origin/main"
end

local function get_pr_file_from_branch(opts?: SpawnOpts): string
  opts = opts or {}
  local do_spawn = opts.spawn or spawn_mod.spawn

  local base_branch = get_default_branch(opts)

  -- Find files added on this branch matching .github/pr/*.md
  local cmd: {string} = {"git"}
  if opts.repo then
    table.insert(cmd, "-C")
    table.insert(cmd, opts.repo)
  end
  table.insert(cmd, "diff")
  table.insert(cmd, "--name-only")
  table.insert(cmd, "--diff-filter=A")
  table.insert(cmd, base_branch .. "...HEAD")
  table.insert(cmd, "--")
  table.insert(cmd, ".github/pr/*.md")

  local handle = do_spawn(cmd)
  local read_result = {handle:read()}
  local ok = read_result[1] as boolean
  local out = read_result[2] as string
  if not ok or not out then
    return nil
  end

  local files: {string} = {}
  for line in (out as string):gmatch("[^\n]+") do
    local name = line:match("^%s*(.-)%s*$")
    if name and name ~= "" then
      table.insert(files, name)
    end
  end

  if #files == 0 then
    log("no .github/pr/*.md files added on this branch")
    return nil
  end

  if #files > 1 then
    log("multiple .github/pr/*.md files found: " .. table.concat(files, ", "))
    return nil
  end

  log("using " .. files[1])
  return files[1]
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

local function default_fetch(url: string, opts?: FetchOpts): FetchResult
  return fetch_mod.Fetch(url, opts as fetch_mod.FetchOpts)
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

  local result = fetch(url, fetch_opts)
  if not result.ok then
    return nil, "fetch failed: " .. result.error
  end

  local decoded_ok, decoded = pcall(cosmo.DecodeJson, result.body)
  if not decoded_ok then
    return nil, "json decode failed: " .. tostring(decoded)
  end

  return result.status, decoded as GitHubPr
end


local function get_pr(owner: string, repo: string, pr_number: integer, token: string, opts?: RequestOpts): GitHubPr, string
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

local function update_pr(owner: string, repo: string, pr_number: integer, title: string, body: string, token: string, opts?: RequestOpts): boolean, string
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
  local help = [[
usage: pr.lua [-h]

Updates PR title and description from .github/pr/<name>.md

The PR file is auto-detected: if the branch adds exactly one .github/pr/*.md
file, that file is used.

The file format:

    # component: verb explanation

    Brief description of changes.

    ## Changes

    - file1.lua - what it does
    - file2.lua - what it does

Guidelines:

  1. Create .github/pr/<descriptive-name>.md on your branch
  2. Use `# component: verb explanation` as the title
  3. Keep content concise but include key decisions and tradeoffs
  4. Update the file as the PR evolves
  5. Push to trigger the workflow and update the PR

Environment variables (set automatically in GitHub Actions):
  GITHUB_TOKEN       - required for API authentication
  GITHUB_REPOSITORY  - owner/repo format
  GITHUB_PR_NUMBER   - PR number
]]
  print(help)
end

local function is_github_actions(): boolean
  return os.getenv("GITHUB_ACTIONS") == "true"
end

local function do_update(owner: string, repo_name: string, pr_number: integer, pr_file: string, token: string, opts?: RequestOpts): integer, string
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

local function main(opts?: MainOpts): integer, string
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
  local pr_number = tonumber(pr_number_str) as integer
  if not pr_number then
    return 1, "invalid GITHUB_PR_NUMBER"
  end

  local spawn_opts: SpawnOpts = {spawn = opts.spawn, repo = opts.repo, getenv = opts.getenv}
  local pr_file = get_pr_file_from_branch(spawn_opts)
  if not pr_file then
    return 0
  end

  local request_opts: RequestOpts = {fetch = opts.fetch, getenv = opts.getenv}
  return do_update(owner, repo_name, pr_number, pr_file, token, request_opts)
end

if cosmo.is_main() then
  if arg[1] == "-h" or arg[1] == "--help" then
    print_help()
    os.exit(0)
  end
  local code, msg = main()
  if msg then
    log(msg)
  end
  os.exit(code or 0)
end

local record PrModule
  record Handle
    pid: number
    read: function(self: Handle): boolean, string
    wait: function(self: Handle): integer
  end

  record FetchOpts
    method: string
    headers: {string:string}
    body: string
  end

  record FetchResult
    ok: boolean
    status: number
    headers: {string:string}
    body: string
    error: string
  end

  record SpawnOpts
    spawn: function({string}): Handle
    repo: string
    getenv: function(string): string
  end

  record RequestOpts
    fetch: function(url: string, opts?: FetchOpts): FetchResult
    getenv: function(string): string
  end

  record MainOpts
    spawn: function({string}): Handle
    repo: string
    getenv: function(string): string
    fetch: function(url: string, opts?: FetchOpts): FetchResult
  end

  record ParsedPr
    title: string
    body: string
  end

  record GitHubPr
    number: number
    title: string
    body: string
    message: string
  end

  parse_pr_md: function(content: string): ParsedPr, string
  github_request: function(method: string, endpoint: string, token: string, body: {string:string}, opts?: RequestOpts): number, GitHubPr
  get_pr: function(owner: string, repo: string, pr_number: integer, token: string, opts?: RequestOpts): GitHubPr, string
  append_timestamp_details: function(body: string): string
  update_pr: function(owner: string, repo: string, pr_number: integer, title: string, body: string, token: string, opts?: RequestOpts): boolean, string
  get_default_branch: function(opts?: SpawnOpts): string
  get_pr_file_from_branch: function(opts?: SpawnOpts): string
  is_github_actions: function(): boolean
  do_update: function(owner: string, repo_name: string, pr_number: integer, pr_file: string, token: string, opts?: RequestOpts): integer, string
  main: function(opts?: MainOpts): integer, string
end

return {
  parse_pr_md = parse_pr_md,
  github_request = github_request,
  get_pr = get_pr,
  append_timestamp_details = append_timestamp_details,
  update_pr = update_pr,
  get_default_branch = get_default_branch,
  get_pr_file_from_branch = get_pr_file_from_branch,
  is_github_actions = is_github_actions,
  do_update = do_update,
  main = main,
} as PrModule
