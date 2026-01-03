#!/usr/bin/env run-test.lua

local pr = require("skill.pr")
local cosmo = require("cosmo")

local function test_append_timestamp_to_new_body()
  local body = "This is the PR description.\n\nSome details here."
  local result = pr.append_timestamp_details(body)

  if not result:match("<!%-%- pr%-update%-history %-%->") then
    io.stderr:write("RESULT:\n" .. result .. "\n")
    error("expected marker comment")
  end
  assert(result:match("<details><summary>Update history</summary>"), "expected details tag")
  assert(result:match("Updated: %d%d%d%d%-%d%d%-%d%dT%d%d:%d%d:%d%dZ"), "expected timestamp")
  assert(result:match("</details>"), "expected closing details tag")
  assert(result:match("This is the PR description"), "expected original body content")
end
test_append_timestamp_to_new_body()

local function test_append_timestamp_to_existing_details()
  local body = [[This is the PR description.

<!-- pr-update-history -->
<details><summary>Update history</summary>

- Updated: 2026-01-01T12:00:00Z
</details>]]

  local result = pr.append_timestamp_details(body)

  -- should have two update entries
  local _, count = result:gsub("Updated: %d%d%d%d%-%d%d%-%d%dT%d%d:%d%d:%d%dZ", "")
  assert(count == 2, "expected two timestamp entries, got " .. count)
  assert(result:match("2026%-01%-01T12:00:00Z"), "expected old timestamp")
end
test_append_timestamp_to_existing_details()

local function test_get_pr_success()
  local mock_fetch = function(url, opts)
    assert(url:match("/repos/owner/repo/pulls/42"), "expected correct URL")
    assert(opts.method == "GET", "expected GET method")
    return 200, {}, cosmo.EncodeJson({
      number = 42,
      title = "Test PR",
      body = "Test body",
    })
  end

  local pr_data, err = pr.get_pr("owner", "repo", 42, "token", {fetch = mock_fetch})
  assert(pr_data, "expected PR data, got error: " .. tostring(err))
  assert(pr_data.number == 42, "expected PR number 42")
  assert(pr_data.title == "Test PR", "expected PR title")
  assert(pr_data.body == "Test body", "expected PR body")
end
test_get_pr_success()

local function test_get_pr_failure()
  local mock_fetch = function()
    return 404, {}, cosmo.EncodeJson({message = "Not Found"})
  end

  local pr_data, err = pr.get_pr("owner", "repo", 42, "token", {fetch = mock_fetch})
  assert(not pr_data, "expected no PR data")
  assert(err and err:match("404"), "expected 404 in error")
  assert(err and err:match("Not Found"), "expected Not Found in error")
end
test_get_pr_failure()

local function test_do_update_with_changes()
  local pr_file = os.tmpname()
  local f = io.open(pr_file, "w")
  f:write("# New Title\n\nNew body content")
  f:close()

  local get_pr_called = false
  local update_pr_called = false
  local captured_body

  local mock_fetch = function(url, opts)
    if opts.method == "GET" then
      get_pr_called = true
      return 200, {}, cosmo.EncodeJson({
        number = 42,
        title = "Old Title",
        body = "Old body content",
      })
    elseif opts.method == "PATCH" then
      update_pr_called = true
      captured_body = cosmo.DecodeJson(opts.body).body
      return 200, {}, cosmo.EncodeJson({number = 42})
    end
  end

  -- override pr_file path for testing
  local path = require("cosmo.path")
  local original_exists = path.exists
  path.exists = function(p)
    if p:match("%.github/pr/%d+%.md") then
      return true
    end
    return original_exists(p)
  end

  local original_slurp = cosmo.Slurp
  cosmo.Slurp = function(p)
    if p:match("%.github/pr/%d+%.md") then
      return "# New Title\n\nNew body content"
    end
    return original_slurp(p)
  end

  local code, err = pr.do_update("owner", "repo", 42, "token", {fetch = mock_fetch})

  path.exists = original_exists
  cosmo.Slurp = original_slurp
  os.remove(pr_file)

  assert(code == 0, "expected success, got error: " .. tostring(err))
  assert(get_pr_called, "expected get_pr to be called")
  assert(update_pr_called, "expected update_pr to be called")
  assert(captured_body:match("New body content"), "expected new body content")
  assert(captured_body:match("Update history"), "expected timestamp details")
  assert(captured_body:match("Updated: %d%d%d%d"), "expected timestamp")
end
test_do_update_with_changes()

local function test_do_update_without_changes()
  local get_pr_called = false
  local update_pr_called = false
  local captured_body

  local mock_fetch = function(url, opts)
    if opts.method == "GET" then
      get_pr_called = true
      return 200, {}, cosmo.EncodeJson({
        number = 42,
        title = "Same Title",
        body = "Same body content",
      })
    elseif opts.method == "PATCH" then
      update_pr_called = true
      captured_body = cosmo.DecodeJson(opts.body).body
      return 200, {}, cosmo.EncodeJson({number = 42})
    end
  end

  local path = require("cosmo.path")
  local original_exists = path.exists
  path.exists = function(p)
    if p:match("%.github/pr/%d+%.md") then
      return true
    end
    return original_exists(p)
  end

  local original_slurp = cosmo.Slurp
  cosmo.Slurp = function(p)
    if p:match("%.github/pr/%d+%.md") then
      return "# Same Title\n\nSame body content"
    end
    return original_slurp(p)
  end

  local code, err = pr.do_update("owner", "repo", 42, "token", {fetch = mock_fetch})

  path.exists = original_exists
  cosmo.Slurp = original_slurp

  assert(code == 0, "expected success, got error: " .. tostring(err))
  assert(get_pr_called, "expected get_pr to be called")
  assert(update_pr_called, "expected update_pr to be called")
  -- when there are no changes, we should NOT append timestamp
  assert(not captured_body:match("Update history"), "expected NO timestamp details when no changes")
end
test_do_update_without_changes()
