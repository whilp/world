#!/usr/bin/env run-test.lua
local fetch = require("cosmic.fetch")

local function test_module_loads()
  assert(fetch ~= nil, "fetch module should not be nil")
  assert(fetch.Fetch ~= nil, "fetch.Fetch should not be nil")
  assert(type(fetch.Fetch) == "function", "fetch.Fetch should be a function")
end
test_module_loads()

local function test_result_structure()
  -- Test that result has correct structure
  -- Use a guaranteed-to-fail protocol to test error handling
  local result = fetch.Fetch("invalid://this-will-definitely-fail")

  assert(result ~= nil, "result should not be nil")
  assert(result.ok == false, "result.ok should be false for failed fetch, got: " .. tostring(result.ok))
  assert(result.status == nil, "result.status should be nil on error")
  assert(result.headers == nil, "result.headers should be nil on error")
  assert(result.body == nil, "result.body should be nil on error")
  assert(result.error ~= nil, "result.error should not be nil on failure")
  assert(type(result.error) == "string", "result.error should be a string")
  assert(#result.error > 0, "result.error should not be empty")
end
test_result_structure()

-- Test with a data URL to verify success path without network dependency
local function test_success_structure()
  -- Use a data URL which should always work
  local result = fetch.Fetch("data:text/plain,Hello%20World")

  -- Note: cosmo.Fetch may or may not support data URLs
  -- If it does, verify success structure
  if result.ok then
    assert(result.status ~= nil, "result.status should be set on success")
    assert(result.headers ~= nil, "result.headers should be set on success")
    assert(type(result.headers) == "table", "result.headers should be a table")
    assert(result.body ~= nil, "result.body should be set on success")
    assert(result.error == nil, "result.error should be nil on success")
  else
    -- If data URLs aren't supported, at least verify error structure
    assert(result.error ~= nil, "result.error should be set on failure")
  end
end
test_success_structure()

-- Test that error messages are preserved (the main point of this wrapper)
local function test_error_preservation()
  local result = fetch.Fetch("invalid://guaranteed-failure")

  -- The key feature: error message should be accessible and non-empty
  assert(result.error ~= nil, "error should be preserved")
  assert(type(result.error) == "string", "error should be a string")
  assert(#result.error > 0, "error should not be empty")
  -- Should not be "nil" string which was the original problem
  assert(result.error ~= "nil", "error should not be 'nil' string")
end
test_error_preservation()

-- Test that HTTP error statuses (4xx, 5xx) still return ok=true
-- because the HTTP request itself succeeded
local function test_http_error_status()
  -- Use httpbin's 404 endpoint - skip if network unavailable
  local result = fetch.Fetch("https://httpbin.org/status/404")

  if not result.ok then
    -- Network failure - skip this test
    io.stderr:write("SKIP: test_http_error_status (network unavailable)\n")
    return
  end

  -- HTTP 404 is still a successful HTTP request
  assert(result.ok == true, "ok should be true for HTTP error status")
  assert(result.status == 404, "status should be 404, got: " .. tostring(result.status))
  assert(result.headers ~= nil, "headers should be present")
  assert(result.error == nil, "error should be nil on HTTP success")
end
test_http_error_status()
