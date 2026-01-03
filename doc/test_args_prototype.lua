-- prototype: ergonomic test argument parsing
-- usage:
--   local test_args = require("test_args")
--   local args = test_args.parse({"manifest_git", "manifest_luafiles"})
--   local files = read_file(args.manifest_git)

local M = {}

-- parse and validate test arguments with names
function M.parse(spec)
  if type(spec) ~= "table" then
    error("test_args.parse: spec must be table of names")
  end

  local args = {}
  local missing = {}

  for i, name in ipairs(spec) do
    if type(name) ~= "string" then
      error(string.format("test_args.parse: spec[%d] must be string, got %s", i, type(name)))
    end

    local value = TEST_ARGS and TEST_ARGS[i]
    if not value then
      table.insert(missing, string.format("[%d] %s", i, name))
    else
      args[name] = value
    end
  end

  if #missing > 0 then
    error(string.format(
      "test_args.parse: missing arguments:\n  %s\n\nexpected %d args: %s",
      table.concat(missing, "\n  "),
      #spec,
      table.concat(spec, ", ")
    ))
  end

  return args
end

-- parse with defaults
function M.parse_with_defaults(spec)
  if type(spec) ~= "table" then
    error("test_args.parse_with_defaults: spec must be table")
  end

  local args = {}
  local i = 1

  for _, item in ipairs(spec) do
    if type(item) == "string" then
      -- required arg
      local value = TEST_ARGS and TEST_ARGS[i]
      if not value then
        error(string.format("test_args.parse: missing required argument [%d] %s", i, item))
      end
      args[item] = value
      i = i + 1
    elseif type(item) == "table" and item.name and item.default then
      -- optional arg with default
      local value = TEST_ARGS and TEST_ARGS[i]
      args[item.name] = value or item.default
      if value then
        i = i + 1
      end
    else
      error("test_args.parse: invalid spec item")
    end
  end

  return args
end

-- example usage
if not pcall(debug.getlocal, 4, 1) then
  -- demo mode
  print("example 1: basic usage")
  print("----------------------")
  _G.TEST_ARGS = {"o/manifest/git.txt", "o/manifest/lua-files.txt", "o/manifest/lua-tests.txt"}

  local args = M.parse({"manifest_git", "manifest_luafiles", "manifest_luatests"})
  print("args.manifest_git:", args.manifest_git)
  print("args.manifest_luafiles:", args.manifest_luafiles)
  print("args.manifest_luatests:", args.manifest_luatests)
  print()

  print("example 2: with defaults")
  print("------------------------")
  _G.TEST_ARGS = {"config.yml"}

  local args2 = M.parse_with_defaults({
    "config_file",
    {name = "rules_dir", default = ".ast-grep/rules"},
  })
  print("args2.config_file:", args2.config_file)
  print("args2.rules_dir:", args2.rules_dir)
  print()

  print("example 3: missing argument error")
  print("----------------------------------")
  _G.TEST_ARGS = {"only-one-arg"}

  local ok, err = pcall(function()
    M.parse({"first", "second", "third"})
  end)
  if not ok then
    print("error (as expected):")
    print(err)
  end
end

return M
