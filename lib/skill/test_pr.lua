#!/usr/bin/env run-test.lua
-- teal ignore: test file

local pr = require("skill.pr")
local cosmo = require("cosmo")
local path = require("cosmo.path")

-- helper: mock spawn that returns predetermined output
local function mock_spawn(output)
  return function()
    return {
      read = function() return true, output end,
      wait = function() return 0 end,
    }
  end
end

--------------------------------------------------------------------------------
-- parsing tests (no external deps)
--------------------------------------------------------------------------------

local function test_parse_basic()
  local result = pr.parse_pr_md("# My Title\n\nBody here.")
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
  local result = pr.parse_pr_md(content)
  assert(result.title == "Feature", "expected title")
  assert(result.body:match("## Changes"), "expected changes section")
end
test_parse_multiline()

local function test_parse_title_only()
  local result = pr.parse_pr_md("# Just a title")
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
  local mock_fetch = function(url, opts)
    assert(opts.headers["Authorization"]:match("Bearer"), "expected bearer")
    return 200, {}, cosmo.EncodeJson({number = 42})
  end
  local status, data = pr.github_request("GET", "/test", "token", nil, {fetch = mock_fetch})
  assert(status == 200, "expected 200")
  assert(data.number == 42, "expected number")
end
test_github_request_success()

local function test_github_request_failure()
  local mock_fetch = function() return nil, "connection refused" end
  local status, err = pr.github_request("GET", "/test", "token", nil, {fetch = mock_fetch})
  assert(not status, "expected no status")
  assert(err:match("fetch failed"), "expected error")
end
test_github_request_failure()

local function test_get_pr()
  local mock_fetch = function()
    return 200, {}, cosmo.EncodeJson({number = 42, title = "Test", body = "Body"})
  end
  local data = pr.get_pr("owner", "repo", 42, "token", {fetch = mock_fetch})
  assert(data.number == 42, "expected pr number")
end
test_get_pr()

local function test_update_pr()
  local captured
  local mock_fetch = function(_, opts)
    captured = cosmo.DecodeJson(opts.body)
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
  local mock_env = {}
  local code = pr.main({getenv = function(k) return mock_env[k] end})
  assert(code == 0, "expected success (help shown)")
end
test_local_mode_shows_help()

--------------------------------------------------------------------------------
-- remote mode: github actions with env vars
--------------------------------------------------------------------------------

local function test_remote_missing_token()
  local mock_env = {GITHUB_ACTIONS = "true", GITHUB_REPOSITORY = "owner/repo"}
  local code, msg = pr.main({getenv = function(k) return mock_env[k] end})
  assert(code == 1, "expected failure")
  assert(msg:match("GITHUB_TOKEN"), "expected token error")
end
test_remote_missing_token()

local function test_remote_missing_repository()
  local mock_env = {GITHUB_ACTIONS = "true", GITHUB_TOKEN = "token"}
  local code, msg = pr.main({getenv = function(k) return mock_env[k] end})
  assert(code == 1, "expected failure")
  assert(msg:match("GITHUB_REPOSITORY"), "expected repo error")
end
test_remote_missing_repository()

local function test_remote_missing_pr_number()
  local mock_env = {GITHUB_ACTIONS = "true", GITHUB_TOKEN = "token", GITHUB_REPOSITORY = "owner/repo"}
  local code, msg = pr.main({getenv = function(k) return mock_env[k] end})
  assert(code == 1, "expected failure")
  assert(msg:match("GITHUB_PR_NUMBER"), "expected pr number error")
end
test_remote_missing_pr_number()

--------------------------------------------------------------------------------
-- trailer extraction (mocked git output)
--------------------------------------------------------------------------------

local function test_trailer_not_found()
  local output = "abc123\ndef456\n"  -- commits with no trailers
  local result = pr.get_pr_name_from_trailer({spawn = mock_spawn(output)})
  assert(result == nil, "expected no trailer")
end
test_trailer_not_found()

local function test_trailer_found()
  local output = "abc123 2026-01-04-feature.md\n"
  local result = pr.get_pr_name_from_trailer({spawn = mock_spawn(output)})
  assert(result == "2026-01-04-feature.md", "expected trailer value, got: " .. tostring(result))
end
test_trailer_found()

local function test_trailer_disabled()
  -- first commit has name, second disables
  local output = "abc123 feature.md\ndef456 false\n"
  local result = pr.get_pr_name_from_trailer({spawn = mock_spawn(output)})
  assert(result == nil, "expected disabled, got: " .. tostring(result))
end
test_trailer_disabled()

local function test_trailer_reenabled()
  -- name -> disable -> new name
  local output = "abc123 old.md\ndef456 false\nghi789 2026-01-04-new.md\n"
  local result = pr.get_pr_name_from_trailer({spawn = mock_spawn(output)})
  assert(result == "2026-01-04-new.md", "expected re-enabled with new name, got: " .. tostring(result))
end
test_trailer_reenabled()

local function test_trailer_info_returned()
  local output = "abc123\ndef456 feature.md\nghi789\n"
  local result, info = pr.get_pr_name_from_trailer({spawn = mock_spawn(output)})
  assert(result == "feature.md", "expected trailer")
  assert(info.commit_count == 3, "expected 3 commits")
  assert(info.first_sha == "abc123", "expected first sha")
  assert(info.last_sha == "ghi789", "expected last sha")
  assert(info.winning_sha == "def456", "expected winning sha")
  assert(info.winning_trailer == "x-cosmic-pr-name: feature.md", "expected winning trailer")
end
test_trailer_info_returned()

--------------------------------------------------------------------------------
-- do_update integration
--------------------------------------------------------------------------------

local function test_do_update_with_changes()
  local mock_fetch = function(url, opts)
    if opts.method == "GET" then
      return 200, {}, cosmo.EncodeJson({number = 42, title = "Old", body = "Old body"})
    else
      return 200, {}, cosmo.EncodeJson({number = 42})
    end
  end

  local original_exists = path.exists
  local original_slurp = cosmo.Slurp
  path.exists = function(p)
    if p:match("test%.md") then return true end
    return original_exists(p)
  end
  cosmo.Slurp = function(p)
    if p:match("test%.md") then return "# New\n\nNew body" end
    return original_slurp(p)
  end

  local code = pr.do_update("owner", "repo", 42, "test.md", nil, "token", {fetch = mock_fetch})

  path.exists = original_exists
  cosmo.Slurp = original_slurp

  assert(code == 0, "expected success")
end
test_do_update_with_changes()
