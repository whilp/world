local lu = require("luaunit")
local pr = require("build.pr")

TestParsePrMd = {}

function TestParsePrMd:test_basic_title_and_body()
  local content = [[# My PR title

This is the body.
]]
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "My PR title")
  lu.assertEquals(result.body, "This is the body.")
end

function TestParsePrMd:test_multiline_body()
  local content = [[# Feature: add new thing

This PR adds a new feature.

## Changes
- Added foo
- Fixed bar

## Testing
Tested manually.
]]
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "Feature: add new thing")
  lu.assertStrContains(result.body, "This PR adds a new feature.")
  lu.assertStrContains(result.body, "## Changes")
  lu.assertStrContains(result.body, "- Added foo")
end

function TestParsePrMd:test_title_only()
  local content = "# Just a title"
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "Just a title")
  lu.assertEquals(result.body, "")
end

function TestParsePrMd:test_title_with_trailing_whitespace()
  local content = "#   Spaced title   \n\nbody"
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "Spaced title")
  lu.assertEquals(result.body, "body")
end

function TestParsePrMd:test_empty_body()
  local content = "# Title\n\n"
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "Title")
  lu.assertEquals(result.body, "")
end

function TestParsePrMd:test_no_title()
  local content = "No hash mark here"
  local result, err = pr.parse_pr_md(content)
  lu.assertNil(result)
  lu.assertStrContains(err, "no title found")
end

function TestParsePrMd:test_empty_content()
  local result, err = pr.parse_pr_md("")
  lu.assertNil(result)
  lu.assertStrContains(err, "no title found")
end

function TestParsePrMd:test_title_with_special_chars()
  local content = "# feat(api): add endpoint for /users\n\nDetails here."
  local result = pr.parse_pr_md(content)
  lu.assertNotNil(result)
  lu.assertEquals(result.title, "feat(api): add endpoint for /users")
  lu.assertEquals(result.body, "Details here.")
end
