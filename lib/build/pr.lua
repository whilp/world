#!/usr/bin/env lua
--[[
Updates a GitHub PR title and description from .github/pr.md

The pr.md format:
  # <title>

  <description>

Environment variables:
  GITHUB_TOKEN - required for API authentication
  GITHUB_REPOSITORY - owner/repo format
  GITHUB_REF_NAME - branch name (used to find PR)
  GITHUB_API_URL - optional, defaults to https://api.github.com

Security notes:
  - File content is used only as PR title/body text (no execution)
  - Token scope should be limited to PR write access
  - Uses unveil to restrict file system access
]]

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local environ = require("environ")

local function log(msg)
  io.stderr:write("pr: " .. msg .. "\n")
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

local function github_request(method, endpoint, token, body)
  local env = environ.new(unix.environ())
  local api_url = env.GITHUB_API_URL or "https://api.github.com"
  local url = api_url .. endpoint

  local opts = {
    method = method,
    headers = {
      ["Authorization"] = "Bearer " .. token,
      ["Accept"] = "application/vnd.github+json",
      ["X-GitHub-Api-Version"] = "2022-11-28",
      ["User-Agent"] = "pr.lua/1.0",
    },
  }

  if body then
    opts.body = cosmo.EncodeJson(body)
    opts.headers["Content-Type"] = "application/json"
  end

  local status, headers, response_body = cosmo.Fetch(url, opts)
  if not status then
    return nil, "fetch failed: " .. tostring(headers)
  end

  local ok, decoded = pcall(cosmo.DecodeJson, response_body)
  if not ok then
    return nil, "json decode failed: " .. tostring(decoded)
  end

  return status, decoded
end

local function find_pr_number(owner, repo, branch, token)
  local endpoint = string.format("/repos/%s/%s/pulls?head=%s:%s&state=open",
    owner, repo, owner, branch)

  local status, data = github_request("GET", endpoint, token)
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

local function update_pr(owner, repo, pr_number, title, body, token)
  local endpoint = string.format("/repos/%s/%s/pulls/%d", owner, repo, pr_number)

  local status, data = github_request("PATCH", endpoint, token, {
    title = title,
    body = body,
  })

  if not status then
    return nil, data
  end

  if status ~= 200 then
    local msg = data and data.message or "unknown error"
    return nil, "api error " .. tostring(status) .. ": " .. msg
  end

  return true
end

local function main()
  local env = environ.new(unix.environ())

  local pr_file = ".github/pr.md"
  local full_path = path.join(unix.getcwd(), pr_file)

  local token = env.GITHUB_TOKEN
  if not token or token == "" then
    log("GITHUB_TOKEN not set, skipping")
    return true
  end

  local repo = env.GITHUB_REPOSITORY
  if not repo then
    log("GITHUB_REPOSITORY not set, skipping")
    return true
  end

  local branch = env.GITHUB_HEAD_REF or env.GITHUB_REF_NAME
  if not branch then
    log("GITHUB_HEAD_REF/GITHUB_REF_NAME not set, skipping")
    return true
  end

  local owner, repo_name = repo:match("^([^/]+)/(.+)$")
  if not owner then
    log("invalid GITHUB_REPOSITORY format: " .. repo)
    return true
  end

  local stat = unix.stat(full_path)
  if not stat then
    log(pr_file .. " not found, skipping")
    return true
  end

  local content = cosmo.Slurp(full_path)
  if not content then
    log("failed to read " .. pr_file)
    return true
  end

  local pr, err = parse_pr_md(content)
  if not pr then
    log("failed to parse " .. pr_file .. ": " .. err)
    return true
  end

  local pr_number
  pr_number, err = find_pr_number(owner, repo_name, branch, token)
  if not pr_number then
    log("could not find PR: " .. err)
    return true
  end

  local ok
  ok, err = update_pr(owner, repo_name, pr_number, pr.title, pr.body, token)
  if not ok then
    log("failed to update PR: " .. err)
    return true
  end

  log("updated PR #" .. pr_number .. ": " .. pr.title)
  return true
end

if not pcall(debug.getlocal, 4, 1) then
  main()
  os.exit(0)
end

return {
  parse_pr_md = parse_pr_md,
}
