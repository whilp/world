#!/usr/bin/env lua
-- Validate manifest lua-files.txt against git ls-files

local spawn = require("cosmic.spawn")
local unix = require("cosmo.unix")
local manifest = require("build.manifest")

local function read_lines(filepath)
  local fd = unix.open(filepath, unix.O_RDONLY)
  if not fd then
    return nil, "failed to open " .. filepath
  end
  local content = unix.read(fd, 1024 * 1024)
  unix.close(fd)
  if not content then
    return nil, "failed to read " .. filepath
  end
  local lines = {}
  for line in content:gmatch("([^\n]+)") do
    if line ~= "" then
      table.insert(lines, line)
    end
  end
  return lines
end

local function validate(txt_file, ok_file)
  local txt_lines, err = read_lines(txt_file)
  if not txt_lines then
    io.stderr:write(err .. "\n")
    return 1
  end

  local expected = manifest.find_lua_files()

  if #txt_lines ~= #expected then
    io.stderr:write(string.format("mismatch: txt has %d files, git has %d\n", #txt_lines, #expected))
    return 1
  end

  for i, path in ipairs(txt_lines) do
    if path ~= expected[i] then
      io.stderr:write(string.format("mismatch at line %d: txt=%s git=%s\n", i, path, expected[i]))
      return 1
    end
  end

  if ok_file then
    unix.makedirs(ok_file:match("(.*/)[^/]+$") or ".")
    local fd = unix.open(ok_file, unix.O_WRONLY + unix.O_CREAT + unix.O_TRUNC, tonumber("644", 8))
    if fd then
      unix.write(fd, "ok\n")
      unix.close(fd)
    end
  end

  return 0
end

local function main(...)
  local args = {...}
  local txt_file = args[1]
  local ok_file = args[2]

  if not txt_file then
    io.stderr:write("usage: manifest-lua-files.lua <txt-file> [ok-file]\n")
    return 1
  end

  return validate(txt_file, ok_file)
end

if not pcall(debug.getlocal, 4, 1) then
  os.exit(main(...))
end

return {
  validate = validate,
}
