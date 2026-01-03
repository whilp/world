#!/usr/bin/env lua
-- analyze actual vs. tracked test dependencies

local function extract_requires(filepath)
  local f = io.open(filepath, "r")
  if not f then return {} end

  local requires = {}
  for line in f:lines() do
    -- match: local x = require("module")
    local mod = line:match('require%s*%(%s*["\']([^"\']+)["\']%s*%)')
    if mod and not mod:match("^luaunit") then
      table.insert(requires, mod)
    end
  end
  f:close()
  return requires
end

local function resolve_module_to_file(mod)
  -- module name -> file path mapping
  local replacements = {
    ["cosmo"] = "builtin (APE)",
    ["cosmo.unix"] = "builtin (APE)",
    ["cosmo.path"] = "builtin (APE)",
  }

  if replacements[mod] then
    return replacements[mod]
  end

  -- try to resolve module path
  -- build.foo -> lib/build/foo.lua
  -- cosmic.spawn -> lib/cosmic/spawn.lua
  local parts = {}
  for part in mod:gmatch("[^.]+") do
    table.insert(parts, part)
  end

  if #parts == 2 then
    return string.format("lib/%s/%s.lua", parts[1], parts[2])
  elseif #parts == 1 then
    return string.format("lib/%s/init.lua or lib/%s.lua", parts[1], parts[1])
  end

  return "unknown"
end

-- analyze a test file
local function analyze_test(test_path)
  print("=== " .. test_path .. " ===")

  local requires = extract_requires(test_path)
  if #requires == 0 then
    print("  no requires found")
    return
  end

  print("  direct requires:")
  for _, mod in ipairs(requires) do
    local file = resolve_module_to_file(mod)
    print(string.format("    %s -> %s", mod, file))
  end
  print()
end

-- analyze all test files
local test_files = {
  "lib/build/test_luafiles.lua",
  "lib/build/test_review.lua",
  "lib/build/test_fetch.lua",
  "lib/build/test_install.lua",
  "lib/cosmic/test_spawn.lua",
  "lib/cosmic/test_walk.lua",
}

for _, test in ipairs(test_files) do
  local f = io.open(test, "r")
  if f then
    f:close()
    analyze_test(test)
  end
end

print("\n=== recommendation ===")
print("add to Makefile:")
print([[
# convention: tests depend on files in same module
$(luatest_o)/lib/%/test_%.ok: lib/%/*.lua

# cross-module dependency: lib/cosmic
lib_cosmic_files := $(wildcard lib/cosmic/*.lua)
$(luatest_o)/lib/build/test_%.ok: $(lib_cosmic_files)
]])
