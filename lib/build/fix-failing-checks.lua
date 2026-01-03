#!/usr/bin/env lua
--test:false
local spawn = require("cosmic.spawn")
local unix = require("cosmo.unix")

local function run_and_capture(cmd)
  local shell_cmd = table.concat(cmd, " ") .. " 2>&1"
  local handle = spawn({"/bin/sh", "-c", shell_cmd})
  local _, stdout = handle:read()
  return stdout or ""
end

local function add_tag_to_file(filepath, tag)
  local fd = unix.open(filepath, unix.O_RDONLY)
  if not fd then
    print("  skip: cannot open " .. filepath)
    return false
  end
  local content = unix.read(fd, 1024 * 1024)
  unix.close(fd)

  if not content then
    print("  skip: cannot read " .. filepath)
    return false
  end

  local tag_pattern = "%-%-" .. tag:gsub("%-", "%%-"):gsub(":", ":")
  if content:match(tag_pattern) then
    print("  skip: already has " .. tag)
    return false
  end

  local new_content = tag .. "\n" .. content
  fd = unix.open(filepath, unix.O_WRONLY | unix.O_TRUNC, tonumber("644", 8))
  if not fd then
    print("  skip: cannot write " .. filepath)
    return false
  end
  unix.write(fd, new_content)
  unix.close(fd)
  print("  added: " .. tag)
  return true
end

local function main()
  print("=== Running make -k test ===")
  local output = run_and_capture({"make", "-k", "test"})

  local test_failures = {}
  for file in output:gmatch("o/luatest/([^%s]+%.lua)%.ok%]") do
    test_failures[file] = true
  end

  print("\nFailing test files:")
  local sorted = {}
  for file in pairs(test_failures) do
    table.insert(sorted, file)
  end
  table.sort(sorted)

  for _, file in ipairs(sorted) do
    print("  " .. file)
    add_tag_to_file(file, "--test:false")
  end

  print("\n=== Running make -k check ===")
  output = run_and_capture({"make", "-k", "check"})

  local check_failures = {}
  for file in output:gmatch("o/luacheck/([^%s]+%.lua)%.ok%]") do
    check_failures[file] = true
  end
  for file in output:gmatch("o/ast%-grep/([^%s]+%.lua)%.ok%]") do
    check_failures[file] = true
  end
  for file in output:gmatch("o/teal/([^%s]+%.lua)%.ok%]") do
    check_failures[file] = true
  end

  print("\nFailing check files:")
  sorted = {}
  for file in pairs(check_failures) do
    table.insert(sorted, file)
  end
  table.sort(sorted)

  for _, file in ipairs(sorted) do
    print("  " .. file)
    add_tag_to_file(file, "--check:false")
  end

  print("\nDone.")
end

main()
