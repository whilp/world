#!/usr/bin/env run-test.lua

local pr = require("skill.pr")
local cosmo = require("cosmo")
local path = require("cosmo.path")

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

local record SpawnHandle
  read: function(self: SpawnHandle): boolean, string
  wait: function(self: SpawnHandle): integer
end

local record SpawnOpts
  spawn: function({string}): SpawnHandle
end

local record MockEnv
  GITHUB_ACTIONS: string
  GITHUB_TOKEN: string
  GITHUB_REPOSITORY: string
  GITHUB_PR_NUMBER: string
end

local record MainOpts
  getenv: function(string): string
  spawn: function({string}): SpawnHandle
  fetch: FetchFn
end

local record FetchOpts
  method: string
  headers: {string:string}
  body: string
end

local type FetchFn = function(url: string, opts?: FetchOpts): number, {string:string}, string

local record ParsedPr
  title: string
  body: string
end

local record TrailerInfo
  commit_count: number
  first_sha: string
  last_sha: string
  winning_sha: string
  winning_trailer: string
end

local record GitHubPr
  number: number
  title: string
  body: string
end

--------------------------------------------------------------------------------
-- mock helpers
--------------------------------------------------------------------------------

local function mock_spawn(output: string): function({string}): SpawnHandle
  return function(_: {string}): SpawnHandle
    return {
      read = function(_: SpawnHandle): boolean, string return true, output end,
      wait = function(_: SpawnHandle): integer return 0 end,
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
  local mock_fetch: FetchFn = function(_: string, opts: FetchOpts): number, {string:string}, string
    assert(opts.headers["Authorization"]:match("Bearer"), "expected bearer")
    return 200, {}, cosmo.EncodeJson({number = 42})
  end
  local status, data = pr.github_request("GET", "/test", "token", nil, {fetch = mock_fetch})
  assert(status == 200, "expected 200")
  local d = data as GitHubPr
  assert(d.number == 42, "expected number")
end
test_github_request_success()

local function test_github_request_failure()
  local mock_fetch: FetchFn = function(_: string, _opts: FetchOpts): number, {string:string}, string return nil, nil, "connection refused" end
  local status, err = pr.github_request("GET", "/test", "token", nil, {fetch = mock_fetch})
  assert(not status, "expected no status")
  assert((err as string):match("fetch failed"), "expected error")
end
test_github_request_failure()

local function test_get_pr()
  local mock_fetch: FetchFn = function(_: string, _opts: FetchOpts): number, {string:string}, string
    return 200, {}, cosmo.EncodeJson({number = 42, title = "Test", body = "Body"})
  end
  local data = pr.get_pr("owner", "repo", 42, "token", {fetch = mock_fetch}) as GitHubPr
  assert(data.number == 42, "expected pr number")
end
test_get_pr()

local function test_update_pr()
  local captured: {string:string}
  local mock_fetch: FetchFn = function(_: string, opts: FetchOpts): number, {string:string}, string
    captured = cosmo.DecodeJson(opts.body) as {string:string}
    return 200, {}, cosmo.EncodeJson({number = 42})
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
-- trailer extraction (mocked git output)
--------------------------------------------------------------------------------

local function test_trailer_not_found()
  local output = "abc123\ndef456\n"  -- commits with no trailers
  local result = pr.get_pr_name_from_trailer({spawn = mock_spawn(output)} as SpawnOpts)
  assert(result == nil, "expected no trailer")
end
test_trailer_not_found()

local function test_trailer_found()
  local output = "abc123 2026-01-04-feature.md\n"
  local result = pr.get_pr_name_from_trailer({spawn = mock_spawn(output)} as SpawnOpts)
  assert(result == "2026-01-04-feature.md", "expected trailer value, got: " .. tostring(result))
end
test_trailer_found()

local function test_trailer_disabled()
  -- first commit has name, second disables
  local output = "abc123 feature.md\ndef456 false\n"
  local result = pr.get_pr_name_from_trailer({spawn = mock_spawn(output)} as SpawnOpts)
  assert(result == nil, "expected disabled, got: " .. tostring(result))
end
test_trailer_disabled()

local function test_trailer_reenabled()
  -- name -> disable -> new name
  local output = "abc123 old.md\ndef456 false\nghi789 2026-01-04-new.md\n"
  local result = pr.get_pr_name_from_trailer({spawn = mock_spawn(output)} as SpawnOpts)
  assert(result == "2026-01-04-new.md", "expected re-enabled with new name, got: " .. tostring(result))
end
test_trailer_reenabled()

local function test_trailer_info_returned()
  local output = "abc123\ndef456 feature.md\nghi789\n"
  local result, info = pr.get_pr_name_from_trailer({spawn = mock_spawn(output)} as SpawnOpts)
  local trailer_info = info as TrailerInfo
  assert(result == "feature.md", "expected trailer")
  assert(trailer_info.commit_count == 3, "expected 3 commits")
  assert(trailer_info.first_sha == "abc123", "expected first sha")
  assert(trailer_info.last_sha == "ghi789", "expected last sha")
  assert(trailer_info.winning_sha == "def456", "expected winning sha")
  assert(trailer_info.winning_trailer == "x-cosmic-pr-name: feature.md", "expected winning trailer")
end
test_trailer_info_returned()

--------------------------------------------------------------------------------
-- do_update integration
--------------------------------------------------------------------------------

local function test_do_update_with_changes()
  local mock_fetch: FetchFn = function(_: string, opts: FetchOpts): number, {string:string}, string
    if opts.method == "GET" then
      return 200, {}, cosmo.EncodeJson({number = 42, title = "Old", body = "Old body"})
    else
      return 200, {}, cosmo.EncodeJson({number = 42})
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

  local code = pr.do_update("owner", "repo", 42, "test.md", nil, "token", {fetch = mock_fetch})

  path.exists = original_exists
  cosmo.Slurp = original_slurp

  assert(code == 0, "expected success")
end
test_do_update_with_changes()

--------------------------------------------------------------------------------
-- trailer without .md extension should still find file
--------------------------------------------------------------------------------

local function test_do_update_finds_file_without_extension()
  local patch_called = false
  local mock_fetch: FetchFn = function(_: string, opts: FetchOpts): number, {string:string}, string
    if opts.method == "GET" then
      return 200, {}, cosmo.EncodeJson({number = 42, title = "Old", body = "Old body"})
    else
      patch_called = true
      return 200, {}, cosmo.EncodeJson({number = 42})
    end
  end

  local original_exists = path.exists
  local original_slurp = cosmo.Slurp
  -- only the .md version exists
  path.exists = function(p: string): boolean
    if p:match("feature%-name%.md$") then return true end
    return false
  end
  cosmo.Slurp = function(p: string): string
    if p:match("feature%-name%.md$") then return "# New\n\nNew body" end
    return nil
  end

  -- trailer value omits the .md extension
  local code = pr.do_update("owner", "repo", 42, "feature-name", nil, "token", {fetch = mock_fetch})

  path.exists = original_exists
  cosmo.Slurp = original_slurp

  assert(code == 0, "expected success when extension omitted")
  assert(patch_called, "expected PR to be updated (PATCH called)")
end
test_do_update_finds_file_without_extension()

--------------------------------------------------------------------------------
-- get_pr_files_from_branch tests
--------------------------------------------------------------------------------

local function test_pr_files_from_branch_none()
  local result = pr.get_pr_files_from_branch({spawn = mock_spawn("")} as SpawnOpts)
  assert(#result == 0, "expected no files")
end
test_pr_files_from_branch_none()

local function test_pr_files_from_branch_one()
  local result = pr.get_pr_files_from_branch({spawn = mock_spawn(".github/pr/feature.md\n")} as SpawnOpts)
  assert(#result == 1, "expected one file")
  assert(result[1] == ".github/pr/feature.md", "expected file path")
end
test_pr_files_from_branch_one()

local function test_pr_files_from_branch_multiple()
  local result = pr.get_pr_files_from_branch({spawn = mock_spawn(".github/pr/a.md\n.github/pr/b.md\n")} as SpawnOpts)
  assert(#result == 2, "expected two files")
end
test_pr_files_from_branch_multiple()

--------------------------------------------------------------------------------
-- main() fallback to branch file detection
--------------------------------------------------------------------------------

local function test_main_fallback_to_branch_file()
  local patch_called = false
  local mock_fetch: FetchFn = function(_: string, opts: FetchOpts): number, {string:string}, string
    if opts.method == "GET" then
      return 200, {}, cosmo.EncodeJson({number = 42, title = "Old", body = "Old body"})
    else
      patch_called = true
      return 200, {}, cosmo.EncodeJson({number = 42})
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
  }

  -- mock spawn to return no trailer but one branch file
  local spawn_calls: {{string}} = {}
  local mock_spawn_fn = function(cmd: {string}): SpawnHandle
    table.insert(spawn_calls, cmd)
    -- first call is trailer check, second is branch file check
    if cmd[#cmd] == ".github/pr/*.md" then
      return {
        read = function(_: SpawnHandle): boolean, string return true, ".github/pr/auto-feature.md\n" end,
        wait = function(_: SpawnHandle): integer return 0 end,
      }
    else
      return {
        read = function(_: SpawnHandle): boolean, string return true, "abc123\ndef456\n" end,  -- no trailers
        wait = function(_: SpawnHandle): integer return 0 end,
      }
    end
  end

  local code = pr.main({
    getenv = function(k: string): string return (mock_env as {string:string})[k] end,
    spawn = mock_spawn_fn,
    fetch = mock_fetch,
  } as MainOpts)

  path.exists = original_exists
  cosmo.Slurp = original_slurp

  assert(code == 0, "expected success")
  assert(patch_called, "expected PR to be updated via fallback")
end
test_main_fallback_to_branch_file()

local function test_main_no_fallback_when_multiple_files()
  local mock_env: MockEnv = {
    GITHUB_ACTIONS = "true",
    GITHUB_TOKEN = "token",
    GITHUB_REPOSITORY = "owner/repo",
    GITHUB_PR_NUMBER = "42",
  }

  local mock_spawn_fn = function(cmd: {string}): SpawnHandle
    if cmd[#cmd] == ".github/pr/*.md" then
      return {
        read = function(_: SpawnHandle): boolean, string return true, ".github/pr/a.md\n.github/pr/b.md\n" end,
        wait = function(_: SpawnHandle): integer return 0 end,
      }
    else
      return {
        read = function(_: SpawnHandle): boolean, string return true, "abc123\n" end,  -- no trailers
        wait = function(_: SpawnHandle): integer return 0 end,
      }
    end
  end

  local code, msg = pr.main({
    getenv = function(k: string): string return (mock_env as {string:string})[k] end,
    spawn = mock_spawn_fn,
  } as MainOpts)

  assert(code == 0, "expected success (skipped)")
  assert(msg:match("no x%-cosmic%-pr%-name"), "expected no trailer message")
end
test_main_no_fallback_when_multiple_files()

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
