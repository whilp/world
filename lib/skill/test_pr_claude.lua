#!/usr/bin/env run-test.lua
-- teal ignore: test file

local pr = require("skill.pr")

-- Test trailer extraction
local function test_get_pr_name_from_trailer()
  local pr_name = pr.get_pr_name_from_trailer()
  -- This test runs in the actual repo, so it should find a trailer or return nil
  -- We just verify the function works without errors
  assert(pr_name == nil or type(pr_name) == "string", "expected nil or string")
end
test_get_pr_name_from_trailer()
