#!/usr/bin/env lua
--[[
Example usage of the curl library

This demonstrates the various ways to use the curl wrapper for HTTP requests.
]]

local curl = require("curl")

-- Example 1: Simple GET request
local function example_get()
  print("Example 1: Simple GET request")
  local response = curl.get("https://httpbin.org/get")
  print("Status:", response.status)
  print("Body:", response.body:sub(1, 100) .. "...")
  print()
end

-- Example 2: GET with query parameters
local function example_get_with_query()
  print("Example 2: GET with query parameters")
  local response = curl.get({
    url = "https://httpbin.org/get",
    query = { foo = "bar", test = "123" }
  })
  print("Status:", response.status)
  print("URL includes query:", response.body:match("foo") ~= nil)
  print()
end

-- Example 3: POST with JSON body
local function example_post_json()
  print("Example 3: POST with JSON body")
  local json = require("3p/json")  -- Using the json library from .local/lib/lua/3p/json.lua

  local response = curl.post({
    url = "https://httpbin.org/post",
    body = json.encode({ key = "value", hello = "world" }),
    headers = { ["Content-Type"] = "application/json" }
  })
  print("Status:", response.status)
  print()
end

-- Example 4: POST with form data
local function example_post_form()
  print("Example 4: POST with form data")
  local response = curl.post({
    url = "https://httpbin.org/post",
    body = { username = "john", password = "secret" }
  })
  print("Status:", response.status)
  print()
end

-- Example 5: Custom headers
local function example_custom_headers()
  print("Example 5: GET with custom headers")
  local response = curl.get({
    url = "https://httpbin.org/headers",
    headers = {
      ["X-Custom-Header"] = "test-value",
      ["User-Agent"] = "My Lua App/1.0"
    }
  })
  print("Status:", response.status)
  print()
end

-- Example 6: Basic authentication
local function example_auth()
  print("Example 6: Basic authentication")
  local response = curl.get({
    url = "https://httpbin.org/basic-auth/user/pass",
    auth = { "user", "pass" }
    -- Or: auth = "user:pass"
  })
  print("Status:", response.status)
  print()
end

-- Example 7: Download to file
local function example_download()
  print("Example 7: Download to file")
  local response = curl.get({
    url = "https://httpbin.org/json",
    output = "/tmp/downloaded.json"
  })
  print("Status:", response.status)
  print("Downloaded to:", "/tmp/downloaded.json")
  print()
end

-- Example 8: Dry run (see the curl command)
local function example_dry_run()
  print("Example 8: Dry run")
  local args = curl.get({
    url = "https://example.com",
    query = { foo = "bar" },
    headers = { ["X-Test"] = "value" },
    dry_run = true
  })
  print("curl command args:", table.concat(args, " "))
  print()
end

-- Example 9: PUT request
local function example_put()
  print("Example 9: PUT request")
  local response = curl.put({
    url = "https://httpbin.org/put",
    body = { data = "updated" }
  })
  print("Status:", response.status)
  print()
end

-- Example 10: DELETE request
local function example_delete()
  print("Example 10: DELETE request")
  local response = curl.delete("https://httpbin.org/delete")
  print("Status:", response.status)
  print()
end

-- Run all examples (commented out to prevent network requests during demo)
-- example_get()
-- example_get_with_query()
-- example_post_json()
-- example_post_form()
-- example_custom_headers()
-- example_auth()
-- example_download()
example_dry_run()
-- example_put()
-- example_delete()

print("To run the examples, uncomment the function calls at the end of this file.")
