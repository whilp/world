#!/usr/bin/env run-test.lua
-- teal ignore: test file

local pr = require("skill.pr")

local function test_basic_title_and_body()
  local content = [[# My PR title

This is the body.
]]
  local result = pr.parse_pr_md(content)
  assert(result, "expected result")
  assert(result.title == "My PR title", "expected title 'My PR title', got: " .. tostring(result.title))
  assert(result.body == "This is the body.", "expected body 'This is the body.', got: " .. tostring(result.body))
end
test_basic_title_and_body()

local function test_multiline_body()
  local content = [[# Feature: add new thing

This PR adds a new feature.

## Changes
- Added foo
- Fixed bar

## Testing
Tested manually.
]]
  local result = pr.parse_pr_md(content)
  assert(result, "expected result")
  assert(result.title == "Feature: add new thing", "expected title")
  assert(result.body:match("This PR adds a new feature%."), "expected body to contain feature description")
  assert(result.body:match("## Changes"), "expected body to contain changes section")
  assert(result.body:match("- Added foo"), "expected body to contain foo")
end
test_multiline_body()

local function test_title_only()
  local content = "# Just a title"
  local result = pr.parse_pr_md(content)
  assert(result, "expected result")
  assert(result.title == "Just a title", "expected title 'Just a title'")
  assert(result.body == "", "expected empty body")
end
test_title_only()

local function test_title_with_trailing_whitespace()
  local content = "#   Spaced title   \n\nbody"
  local result = pr.parse_pr_md(content)
  assert(result, "expected result")
  assert(result.title == "Spaced title", "expected trimmed title")
  assert(result.body == "body", "expected trimmed body")
end
test_title_with_trailing_whitespace()

local function test_empty_body()
  local content = "# Title\n\n"
  local result = pr.parse_pr_md(content)
  assert(result, "expected result")
  assert(result.title == "Title", "expected title")
  assert(result.body == "", "expected empty body")
end
test_empty_body()

local function test_no_title()
  local content = "No hash mark here"
  local result, err = pr.parse_pr_md(content)
  assert(not result, "expected no result")
  assert(err and err:match("no title found"), "expected error about no title")
end
test_no_title()

local function test_empty_content()
  local result, err = pr.parse_pr_md("")
  assert(not result, "expected no result")
  assert(err and err:match("no title found"), "expected error about no title")
end
test_empty_content()

local function test_title_with_special_chars()
  local content = "# feat(api): add endpoint for /users\n\nDetails here."
  local result = pr.parse_pr_md(content)
  assert(result, "expected result")
  assert(result.title == "feat(api): add endpoint for /users", "expected title with special chars")
  assert(result.body == "Details here.", "expected body")
end
test_title_with_special_chars()
