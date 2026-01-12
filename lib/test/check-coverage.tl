#!/usr/bin/env lua
-- check-coverage: verify all test files are declared in module _tests variables

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local record Stat
  mode: function(self): number
end

local record DirHandle
  read: function(self): string
  close: function(self)
end

-- find all test_*.tl files in a directory tree
local function find_test_files(dir: string, files?: {string}): {string}
  files = files or {}
  local handle = unix.opendir(dir) as DirHandle
  if not handle then return files end

  while true do
    local entry = handle:read()
    if not entry then break end
    if entry ~= "." and entry ~= ".." then
      local full_path = path.join(dir, entry)
      local stat = unix.stat(full_path) as Stat
      if stat then
        if unix.S_ISDIR(stat:mode()) then
          find_test_files(full_path, files)
        elseif entry:match("^test_.*%.tl$") then
          table.insert(files, full_path)
        end
      end
    end
  end
  handle:close()
  return files
end

-- check if a test file has --test:false marker
local function is_test_disabled(filepath: string): boolean
  local content = cosmo.Slurp(filepath)
  if not content then return false end
  -- check first few lines for --test:false marker
  local lines = 0
  for line in content:gmatch("[^\n]+") do
    lines = lines + 1
    if lines > 10 then break end
    if line:match("%-%-test:false") then
      return true
    end
  end
  return false
end

local function main(declared_tests: {string}): number
  -- build set of declared tests from arguments
  local declared: {string:boolean} = {}
  for _, test in ipairs(declared_tests) do
    declared[test] = true
  end

  -- find all test files
  local test_files: {string} = {}
  for _, dir in ipairs({"lib", "3p"}) do
    find_test_files(dir, test_files)
  end
  table.sort(test_files)

  -- check for undeclared tests
  local missing: {string} = {}
  for _, test in ipairs(test_files) do
    -- skip files with --test:false marker
    if is_test_disabled(test) then
      -- intentionally disabled, skip
    elseif not declared[test] then
      table.insert(missing, test)
    end
  end

  -- report results
  if #missing > 0 then
    io.write("fail: " .. #missing .. " test file(s) not declared in any module's _tests\n\n")
    for _, test in ipairs(missing) do
      io.write("  " .. test .. "\n")
    end
    io.write("\nAdd to appropriate module's _tests variable in cook.mk\n")
    return 1
  else
    io.write("pass: all " .. #test_files .. " test files are declared\n")
    return 0
  end
end

if cosmo.is_main() then
  os.exit(main(arg))
end
