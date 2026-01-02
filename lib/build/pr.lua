--[[
Updates a GitHub PR title and description from .github/pr/<number>.md

The pr.md format:
  # <title>

  <description>

Environment variables:
  GITHUB_TOKEN - required for API authentication
  GITHUB_REPOSITORY - owner/repo format
  GITHUB_PR_NUMBER - PR number (set by workflow)
  GITHUB_HEAD_REF - branch name (fallback to find PR)
  GITHUB_API_URL - optional, defaults to https://api.github.com
]]

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")

local function log(msg)
  io.stderr:write("pr: " .. msg .. "\n")
end

local function get_current_branch()
  local handle = spawn({"git", "rev-parse", "--abbrev-ref", "HEAD"})
  local ok, out = handle:read()
  if not ok then
    return nil
  end
  local branch = out:match("^%s*(.-)%s*$")
  if branch == "" then
    return nil
  end
  return branch
end

local function parse_pr_md(content)
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

local function default_fetch(url, opts)
  return cosmo.Fetch(url, opts)
end

local function github_request(method, endpoint, token, body, opts)
  opts = opts or {}
  local fetch = opts.fetch or default_fetch
  local getenv = opts.getenv or os.getenv

  local api_url = getenv("GITHUB_API_URL") or "https://api.github.com"
  local url = api_url .. endpoint

  local fetch_opts = {
    method = method,
    headers = {
      ["Authorization"] = "Bearer " .. token,
      ["Accept"] = "application/vnd.github+json",
      ["X-GitHub-Api-Version"] = "2022-11-28",
      ["User-Agent"] = "pr.lua/1.0",
    },
  }

  if body then
    fetch_opts.body = cosmo.EncodeJson(body)
    fetch_opts.headers["Content-Type"] = "application/json"
  end

  local status, headers, response_body = fetch(url, fetch_opts)
  if not status then
    return nil, "fetch failed: " .. tostring(headers)
  end

  local ok, decoded = pcall(cosmo.DecodeJson, response_body)
  if not ok then
    return nil, "json decode failed: " .. tostring(decoded)
  end

  return status, decoded
end

local function find_pr_number(owner, repo, branch, token, opts)
  local endpoint = string.format("/repos/%s/%s/pulls?head=%s:%s&state=open",
    owner, repo, owner, branch)

  local status, data = github_request("GET", endpoint, token, nil, opts)
  if not status then
    return nil, data
  end

  if status ~= 200 then
    return nil, "api error: " .. tostring(status)
  end

  if type(data) ~= "table" or #data == 0 then
    return nil, "no open PR found for branch: " .. branch
  end

  return data[1].number
end

local function update_pr(owner, repo, pr_number, title, body, token, opts)
  local endpoint = string.format("/repos/%s/%s/pulls/%d", owner, repo, pr_number)

  local status, data = github_request("PATCH", endpoint, token, {
    title = title,
    body = body,
  }, opts)

  if not status then
    return nil, data
  end

  if status ~= 200 then
    local msg = data and data.message or "unknown error"
    return nil, "api error " .. tostring(status) .. ": " .. msg
  end

  return true
end

local function get_pr_number_from_env(opts)
  opts = opts or {}
  local getenv = opts.getenv or os.getenv

  local pr_number = getenv("GITHUB_PR_NUMBER")
  if pr_number and pr_number ~= "" then
    return tonumber(pr_number)
  end

  local token = getenv("GITHUB_TOKEN")
  if not token or token == "" then
    return nil, "GITHUB_TOKEN not set"
  end

  local repo = getenv("GITHUB_REPOSITORY")
  if not repo then
    return nil, "GITHUB_REPOSITORY not set"
  end

  local branch = getenv("GITHUB_HEAD_REF") or getenv("GITHUB_REF_NAME")
  if not branch then
    branch = get_current_branch()
  end
  if not branch then
    return nil, "could not determine branch"
  end

  local owner, repo_name = repo:match("^([^/]+)/(.+)$")
  if not owner then
    return nil, "invalid GITHUB_REPOSITORY format"
  end

  return find_pr_number(owner, repo_name, branch, token, opts)
end

local function main(opts)
  opts = opts or {}
  local getenv = opts.getenv or os.getenv

  local token = getenv("GITHUB_TOKEN")
  if not token or token == "" then
    return 1, "GITHUB_TOKEN not set, skipping"
  end

  local repo = getenv("GITHUB_REPOSITORY")
  if not repo then
    return 1, "GITHUB_REPOSITORY not set, skipping"
  end

  local owner, repo_name = repo:match("^([^/]+)/(.+)$")
  if not owner then
    return 1, "invalid GITHUB_REPOSITORY format: " .. repo
  end

  local pr_number, err = get_pr_number_from_env(opts)
  if not pr_number then
    return 1, "could not determine PR number: " .. err
  end

  local pr_file = string.format(".github/pr/%d.md", pr_number)
  local full_path = unix.realpath(pr_file)

  if not full_path then
    return 1, pr_file .. " not found, skipping"
  end

  local stat = unix.stat(full_path)
  if not stat then
    return 1, pr_file .. " not found, skipping"
  end

  local content = cosmo.Slurp(full_path)
  if not content then
    return 1, "failed to read " .. pr_file
  end

  local pr
  pr, err = parse_pr_md(content)
  if not pr then
    return 1, "failed to parse " .. pr_file .. ": " .. err
  end

  local ok
  ok, err = update_pr(owner, repo_name, pr_number, pr.title, pr.body, token, opts)
  if not ok then
    return 1, "failed to update PR: " .. err
  end

  log("updated PR #" .. pr_number .. ": " .. pr.title)
  return 0
end

local function print_help()
  local pr_number, _ = get_pr_number_from_env()
  local pr_file = pr_number and string.format(".github/pr/%d.md", pr_number) or ".github/pr/<number>.md"

  local help = string.format([[
usage: pr.lua [-h]

Updates PR title and description from %s

The file format:

    # component: verb explanation

    Brief description of changes.

    - file1.lua - what it does
    - file2.lua - what it does

    ## Validation

    - [x] tests pass
    - [x] linter passes

Guidelines:

  1. Write PR description in %s
  2. Follow repo conventions: `# component: verb explanation` title
  3. Keep content concise but include key decisions, tradeoffs, examples
  4. Update the file as the PR evolves
  5. Push to trigger the workflow and update the PR

Environment variables (set automatically in GitHub Actions):
  GITHUB_TOKEN       - required for API authentication
  GITHUB_REPOSITORY  - owner/repo format
  GITHUB_PR_NUMBER   - PR number
  GITHUB_HEAD_REF    - PR source branch name (fallback)
]], pr_file, pr_file)

  if pr_number then
    help = help .. string.format("\nDetected PR: #%d\n", pr_number)
  end

  print(help)
end

if not pcall(debug.getlocal, 4, 1) then
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

return {
  parse_pr_md = parse_pr_md,
  github_request = github_request,
  find_pr_number = find_pr_number,
  update_pr = update_pr,
  get_current_branch = get_current_branch,
  get_pr_number_from_env = get_pr_number_from_env,
  main = main,
}
