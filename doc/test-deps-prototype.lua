#!/usr/bin/env lua
-- prototype: test dependency extraction

-- example test with dependency declaration
local example_test = [[
local lu = require("luaunit")
local review = require("build.review")

-- declare test file dependencies
local function test_dependencies()
  return {
    "lib/build/review.lua",
    "$(o_any)/build/lib/build/review.lua",
  }
end

function test_something()
  lu.assertEquals(1, 1)
end

-- export deps for build system
if TEST_DEPS_MODE then
  return {dependencies = test_dependencies}
end
]]

-- extract dependencies from test file
local function extract_deps(test_path)
  -- set flag so test exports deps instead of running
  _G.TEST_DEPS_MODE = true

  local chunk, err = loadfile(test_path)
  if not chunk then
    return nil, err
  end

  local ok, result = pcall(chunk)
  if not ok then
    return nil, result
  end

  if type(result) == "table" and type(result.dependencies) == "function" then
    local deps = result.dependencies()
    return deps
  end

  return {}  -- no deps declared
end

-- generate .deps file in make format
local function generate_deps_file(test_path, deps_path, ok_path)
  local deps = extract_deps(test_path)
  if not deps then
    return  -- no deps to generate
  end

  local f = io.open(deps_path, "w")
  for _, dep in ipairs(deps) do
    f:write(ok_path .. ": " .. dep .. "\n")
  end
  f:close()
end

-- example usage
local test_path = "lib/build/test_review.lua"
local ok_path = "o/luatest/lib/build/test_review.lua.ok"
local deps_path = "o/luatest/lib/build/test_review.lua.deps"

print("example .deps file content:")
print("---")
for _, dep in ipairs({"lib/build/review.lua", "$(o_any)/build/lib/build/review.lua"}) do
  print(ok_path .. ": " .. dep)
end
print("---")

-- alternative: simpler annotation approach
print("\nalternative annotation approach:")
print("# in test file:")
print("--depends: lib/build/review.lua")
print("--depends: $(o_any)/build/lib/build/review.lua")
print("\n# extract with simple grep:")
print("$ grep '^--depends:' test.lua | sed 's/--depends: /target: /' > target.deps")
