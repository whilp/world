-- teal ignore: type annotations needed
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

local function get_pr_name_from_trailer()
  local handle = spawn({"git", "log", "-1", "--format=%(trailers:key=x-cosmic-pr-name,valueonly)"})
  local ok, out = handle:read()
  if not ok or not out then
    return nil
  end
  local name = out:match("^%s*(.-)%s*$")
  if name == "" then
    return nil
  end
  return name
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

  local ok, decoded = pcall(cosmo.DecodeJson, response_body)
  if not ok then
    return nil, "json decode failed: " .. tostring(decoded)
  end

  return status, decoded
end


local function get_pr(owner, repo, pr_number, token, opts)
  local endpoint = string.format("/repos/%s/%s/pulls/%d", owner, repo, pr_number)

  local status, data = github_request("GET", endpoint, token, nil, opts)
  if not status then
    return nil, data
  end

  if status ~= 200 then
    local msg = data and data.message or "unknown error"
    return nil, "api error " .. tostring(status) .. ": " .. msg
  end

  return data
end

local function append_timestamp_details(body)
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


local function print_help()
  local pr_name = get_pr_name_from_trailer()
  local pr_file = pr_name and path.join(".github/pr", pr_name) or ".github/pr/<name>.md"

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

local function is_github_actions()
  return os.getenv("GITHUB_ACTIONS") == "true"
end

local function do_update(owner, repo_name, pr_number, pr_name, token, opts)
  local pr_file = path.join(".github/pr", pr_name)

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
  local current_pr
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

  local ok
  ok, err = update_pr(owner, repo_name, pr_number, pr.title, body_to_update, token, opts)
  if not ok then
    return 1, "failed to update PR: " .. err
  end

  log("updated PR #" .. pr_number .. ": " .. pr.title)
  return 0
end

local function main(opts)
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

  local pr_number = getenv("GITHUB_PR_NUMBER")
  if not pr_number or pr_number == "" then
    return 1, "GITHUB_PR_NUMBER not set"
  end
  pr_number = tonumber(pr_number)
  if not pr_number then
    return 1, "invalid GITHUB_PR_NUMBER"
  end

  local pr_name = get_pr_name_from_trailer()
  if not pr_name then
    return 1, "no x-cosmic-pr-name trailer found in HEAD commit"
  end

  return do_update(owner, repo_name, pr_number, pr_name, token, opts)
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

return {
  parse_pr_md = parse_pr_md,
  github_request = github_request,
  get_pr = get_pr,
  append_timestamp_details = append_timestamp_details,
  update_pr = update_pr,
  get_current_branch = get_current_branch,
  get_pr_name_from_trailer = get_pr_name_from_trailer,
  is_github_actions = is_github_actions,
  do_update = do_update,
  main = main,
}
