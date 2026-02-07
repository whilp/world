#!/usr/bin/env run-test.lua

local pr = require("skill.pr")
local cosmo = require("cosmo")
local path = require("cosmo.path")

--------------------------------------------------------------------------------
-- types imported from pr module
--------------------------------------------------------------------------------

local type Handle = pr.Handle
local type SpawnOpts = pr.SpawnOpts
local type MainOpts = pr.MainOpts
local type FetchOpts = pr.FetchOpts
local type FetchResult = pr.FetchResult
local type ParsedPr = pr.ParsedPr
local type GitHubPr = pr.GitHubPr

local type FetchFn = function(url: string, opts?: FetchOpts): FetchResult

local record MockEnv
  GITHUB_ACTIONS: string
  GITHUB_TOKEN: string
  GITHUB_REPOSITORY: string
  GITHUB_PR_NUMBER: string
  GITHUB_BASE_REF: string
end

--------------------------------------------------------------------------------
-- mock helpers
--------------------------------------------------------------------------------

local function mock_spawn(output: string): function({string}): Handle
  return function(_: {string}): Handle
    return {
      read = function(_: Handle): boolean, string return true, output end,
      wait = function(_: Handle): integer return 0 end,
    }
  end
end

--------------------------------------------------------------------------------
-- parsing tests (no external deps)
--------------------------------------------------------------------------------

local function test_parse_basic()
  local result = pr.parse_pr_md("# My Title\n\nBody here.") as ParsedPr
  assert(result.title == "My Title", "expected title")
  assert(result.body == "Body here.", "expected body")
end
test_parse_basic()

local function test_parse_multiline()
  local content = [[# Feature

Description.

## Changes
- Added foo
]]
  local result = pr.parse_pr_md(content) as ParsedPr
  assert(result.title == "Feature", "expected title")
  assert(result.body:match("## Changes"), "expected changes section")
end
test_parse_multiline()

local function test_parse_title_only()
  local result = pr.parse_pr_md("# Just a title") as ParsedPr
  assert(result.title == "Just a title", "expected title")
  assert(result.body == "", "expected empty body")
end
test_parse_title_only()

local function test_parse_no_title()
  local result, err = pr.parse_pr_md("No hash mark")
  assert(not result, "expected no result")
  assert(err:match("no title"), "expected error")
end
test_parse_no_title()

--------------------------------------------------------------------------------
-- timestamp tests
--------------------------------------------------------------------------------

local function test_timestamp_new_body()
  local result = pr.append_timestamp_details("Body content.")
  assert(result:match("<!%-%- pr%-update%-history %-%->"), "expected marker")
  assert(result:match("<details>"), "expected details tag")
  assert(result:match("Updated: %d%d%d%d%-%d%d%-%d%d"), "expected timestamp")
end
test_timestamp_new_body()

local function test_timestamp_replace_existing()
  local body = [[Body.

<!-- pr-update-history -->
<details><summary>Update history</summary>

- Updated: 2026-01-01T00:00:00Z
</details>]]

  local result = pr.append_timestamp_details(body)
  local _, count = result:gsub("Updated:", "")
  assert(count == 1, "expected one timestamp")
  assert(not result:match("2026%-01%-01"), "expected old timestamp replaced")
end
test_timestamp_replace_existing()

--------------------------------------------------------------------------------
-- github api tests (mocked)
--------------------------------------------------------------------------------

local function test_github_request_success()
  local mock_fetch: FetchFn = function(_: string, opts: FetchOpts): FetchResult
    assert(opts.headers["Authorization"]:match("Bearer"), "expected bearer")
    return {ok = true, status = 200, headers = {}, body = cosmo.EncodeJson({number = 42})}
  end
  local status, data = pr.github_request("GET", "/test", "token", nil, {fetch = mock_fetch})
  assert(status == 200, "expected 200")
  local d = data as GitHubPr
  assert(d.number == 42, "expected number")
end
test_github_request_success()

local function test_github_request_failure()
  local mock_fetch: FetchFn = function(_: string, _opts: FetchOpts): FetchResult
    return {ok = false, error = "connection refused"}
  end
  local status, err = pr.github_request("GET", "/test", "token", nil, {fetch = mock_fetch})
  assert(not status, "expected no status")
  assert((err as string):match("fetch failed"), "expected error")
end
test_github_request_failure()

local function test_get_pr()
  local mock_fetch: FetchFn = function(_: string, _opts: FetchOpts): FetchResult
    return {ok = true, status = 200, headers = {}, body = cosmo.EncodeJson({number = 42, title = "Test", body = "Body"})}
  end
  local data = pr.get_pr("owner", "repo", 42, "token", {fetch = mock_fetch}) as GitHubPr
  assert(data.number == 42, "expected pr number")
end
test_get_pr()

local function test_update_pr()
  local captured: {string:string}
  local mock_fetch: FetchFn = function(_: string, opts: FetchOpts): FetchResult
    captured = cosmo.DecodeJson(opts.body) as {string:string}
    return {ok = true, status = 200, headers = {}, body = cosmo.EncodeJson({number = 42})}
  end
  local ok = pr.update_pr("owner", "repo", 42, "Title", "Body", "token", {fetch = mock_fetch})
  assert(ok, "expected success")
  assert(captured.title == "Title", "expected title")
end
test_update_pr()

--------------------------------------------------------------------------------
-- local mode: shows help when not in github actions
--------------------------------------------------------------------------------

local function test_local_mode_shows_help()
  local mock_env: MockEnv = {}
  local code = pr.main({getenv = function(k: string): string return (mock_env as {string:string})[k] end})
  assert(code == 0, "expected success (help shown)")
end
test_local_mode_shows_help()

--------------------------------------------------------------------------------
-- remote mode: github actions with env vars
--------------------------------------------------------------------------------

local function test_remote_missing_token()
  local mock_env: MockEnv = {GITHUB_ACTIONS = "true", GITHUB_REPOSITORY = "owner/repo"}
  local code, msg = pr.main({getenv = function(k: string): string return (mock_env as {string:string})[k] end})
  assert(code == 1, "expected failure")
  assert(msg:match("GITHUB_TOKEN"), "expected token error")
end
test_remote_missing_token()

local function test_remote_missing_repository()
  local mock_env: MockEnv = {GITHUB_ACTIONS = "true", GITHUB_TOKEN = "token"}
  local code, msg = pr.main({getenv = function(k: string): string return (mock_env as {string:string})[k] end})
  assert(code == 1, "expected failure")
  assert(msg:match("GITHUB_REPOSITORY"), "expected repo error")
end
test_remote_missing_repository()

local function test_remote_missing_pr_number()
  local mock_env: MockEnv = {GITHUB_ACTIONS = "true", GITHUB_TOKEN = "token", GITHUB_REPOSITORY = "owner/repo"}
  local code, msg = pr.main({getenv = function(k: string): string return (mock_env as {string:string})[k] end})
  assert(code == 1, "expected failure")
  assert(msg:match("GITHUB_PR_NUMBER"), "expected pr number error")
end
test_remote_missing_pr_number()

--------------------------------------------------------------------------------
-- get_pr_file_from_branch tests
--------------------------------------------------------------------------------

local function test_pr_file_from_branch_none()
  local result = pr.get_pr_file_from_branch({spawn = mock_spawn("")} as SpawnOpts)
  assert(result == nil, "expected nil when no files")
end
test_pr_file_from_branch_none()

local function test_pr_file_from_branch_one()
  local result = pr.get_pr_file_from_branch({spawn = mock_spawn(".github/pr/feature.md\n")} as SpawnOpts)
  assert(result == ".github/pr/feature.md", "expected file path")
end
test_pr_file_from_branch_one()

local function test_pr_file_from_branch_multiple()
  local result = pr.get_pr_file_from_branch({spawn = mock_spawn(".github/pr/a.md\n.github/pr/b.md\n")} as SpawnOpts)
  assert(result == nil, "expected nil when multiple files")
end
test_pr_file_from_branch_multiple()

--------------------------------------------------------------------------------
-- do_update integration
--------------------------------------------------------------------------------

local function test_do_update_with_changes()
  local mock_fetch: FetchFn = function(_: string, opts: FetchOpts): FetchResult
    if opts.method == "GET" then
      return {ok = true, status = 200, headers = {}, body = cosmo.EncodeJson({number = 42, title = "Old", body = "Old body"})}
    else
      return {ok = true, status = 200, headers = {}, body = cosmo.EncodeJson({number = 42})}
    end
  end

  local original_exists = path.exists
  local original_slurp = cosmo.Slurp
  path.exists = function(p: string): boolean
    if p:match("test%.md") then return true end
    return original_exists(p)
  end
  cosmo.Slurp = function(p: string): string
    if p:match("test%.md") then return "# New\n\nNew body" end
    return original_slurp(p)
  end

  local code = pr.do_update("owner", "repo", 42, ".github/pr/test.md", "token", {fetch = mock_fetch})

  path.exists = original_exists
  cosmo.Slurp = original_slurp

  assert(code == 0, "expected success")
end
test_do_update_with_changes()

--------------------------------------------------------------------------------
-- main() with file detection
--------------------------------------------------------------------------------

local function test_main_with_single_file()
  local patch_called = false
  local mock_fetch: FetchFn = function(_: string, opts: FetchOpts): FetchResult
    if opts.method == "GET" then
      return {ok = true, status = 200, headers = {}, body = cosmo.EncodeJson({number = 42, title = "Old", body = "Old body"})}
    else
      patch_called = true
      return {ok = true, status = 200, headers = {}, body = cosmo.EncodeJson({number = 42})}
    end
  end

  local original_exists = path.exists
  local original_slurp = cosmo.Slurp
  path.exists = function(p: string): boolean
    if p:match("auto%-feature%.md$") then return true end
    return false
  end
  cosmo.Slurp = function(p: string): string
    if p:match("auto%-feature%.md$") then return "# Auto Feature\n\nBody" end
    return nil
  end

  local mock_env: MockEnv = {
    GITHUB_ACTIONS = "true",
    GITHUB_TOKEN = "token",
    GITHUB_REPOSITORY = "owner/repo",
    GITHUB_PR_NUMBER = "42",
    GITHUB_BASE_REF = "main",
  }

  local code = pr.main({
    getenv = function(k: string): string return (mock_env as {string:string})[k] end,
    spawn = mock_spawn(".github/pr/auto-feature.md\n"),
    fetch = mock_fetch,
  } as MainOpts)

  path.exists = original_exists
  cosmo.Slurp = original_slurp

  assert(code == 0, "expected success")
  assert(patch_called, "expected PR to be updated")
end
test_main_with_single_file()

local function test_main_no_files()
  local mock_env: MockEnv = {
    GITHUB_ACTIONS = "true",
    GITHUB_TOKEN = "token",
    GITHUB_REPOSITORY = "owner/repo",
    GITHUB_PR_NUMBER = "42",
    GITHUB_BASE_REF = "main",
  }

  local code = pr.main({
    getenv = function(k: string): string return (mock_env as {string:string})[k] end,
    spawn = mock_spawn(""),  -- no files
  } as MainOpts)

  assert(code == 0, "expected success (skipped)")
end
test_main_no_files()

local function test_main_multiple_files()
  local mock_env: MockEnv = {
    GITHUB_ACTIONS = "true",
    GITHUB_TOKEN = "token",
    GITHUB_REPOSITORY = "owner/repo",
    GITHUB_PR_NUMBER = "42",
    GITHUB_BASE_REF = "main",
  }

  local code = pr.main({
    getenv = function(k: string): string return (mock_env as {string:string})[k] end,
    spawn = mock_spawn(".github/pr/a.md\n.github/pr/b.md\n"),
  } as MainOpts)

  assert(code == 0, "expected success (skipped)")
end
test_main_multiple_files()

--------------------------------------------------------------------------------
-- get_default_branch tests
--------------------------------------------------------------------------------

local function test_default_branch_from_github_base_ref()
  local mock_getenv = function(name: string): string
    if name == "GITHUB_BASE_REF" then return "develop" end
    return nil
  end

  local result = pr.get_default_branch({getenv = mock_getenv} as SpawnOpts)
  assert(result == "origin/develop", "expected origin/develop from GITHUB_BASE_REF, got: " .. tostring(result))
end
test_default_branch_from_github_base_ref()

local function test_default_branch_fallback_without_env()
  local mock_getenv = function(_: string): string
    return nil
  end

  local result = pr.get_default_branch({getenv = mock_getenv} as SpawnOpts)
  assert(result == "origin/main", "expected origin/main fallback, got: " .. tostring(result))
end
test_default_branch_fallback_without_env()
