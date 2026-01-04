#!/usr/bin/env run-test.lua

local cosmic = require("skill.cosmic")

local function test_format_check_logs_with_failures()
  local check_runs = {
    {id = 1, name = "test", status = "completed", conclusion = "success"},
    {id = 2, name = "lint", status = "completed", conclusion = "failure"},
    {id = 3, name = "build", status = "completed", conclusion = "failure"},
  }

  local log_calls = {}
  local mock_get_logs = function(owner, repo, run_id, token, opts)
    table.insert(log_calls, run_id)
    if run_id == 2 then
      return "lint error at line 42"
    elseif run_id == 3 then
      return "build failed: missing dependency"
    end
    return nil
  end

  local formatted = cosmic.format_check_logs(check_runs, "owner", "repo", "token", {get_logs = mock_get_logs})

  assert(formatted, "expected formatted logs")
  assert(formatted:match("lint"), "expected lint check")
  assert(formatted:match("failure"), "expected failure status")
  assert(formatted:match("lint error at line 42"), "expected lint logs")
  assert(formatted:match("build"), "expected build check")
  assert(formatted:match("missing dependency"), "expected build logs")
  assert(#log_calls == 2, "expected logs to be fetched for 2 failed checks")
end
test_format_check_logs_with_failures()

local function test_format_check_logs_all_success()
  local check_runs = {
    {id = 1, name = "test", status = "completed", conclusion = "success"},
    {id = 2, name = "lint", status = "completed", conclusion = "success"},
  }

  local formatted = cosmic.format_check_logs(check_runs, "owner", "repo", "token", {})
  assert(not formatted, "expected no formatted logs for all successes")
end
test_format_check_logs_all_success()

local function test_format_check_logs_no_logs_available()
  local check_runs = {
    {id = 1, name = "test", status = "completed", conclusion = "failure"},
  }

  local mock_get_logs = function(owner, repo, run_id, token, opts)
    return nil
  end

  local formatted = cosmic.format_check_logs(check_runs, "owner", "repo", "token", {get_logs = mock_get_logs})

  assert(formatted, "expected formatted output even without logs")
  assert(formatted:match("no logs available"), "expected 'no logs available' message")
end
test_format_check_logs_no_logs_available()

local function test_format_check_logs_in_progress()
  local check_runs = {
    {id = 1, name = "test", status = "in_progress", conclusion = nil},
    {id = 2, name = "lint", status = "completed", conclusion = "failure"},
  }

  local log_count = 0
  local mock_get_logs = function(owner, repo, run_id, token, opts)
    log_count = log_count + 1
    return "some logs"
  end

  local formatted = cosmic.format_check_logs(check_runs, "owner", "repo", "token", {get_logs = mock_get_logs})

  assert(formatted, "expected formatted logs")
  assert(not formatted:match("in_progress"), "expected in_progress check to be skipped")
  assert(log_count == 1, "expected logs to be fetched only for completed failures")
end
test_format_check_logs_in_progress()
