local luaunit = require("luaunit")
local spawn = require("spawn").spawn
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local cosmo = require("cosmo")

local lua_dist = path.join(os.getenv("TEST_BIN_DIR"):gsub("/tl$", "/lua"), "bin", "lua.dist")
local tl_bin = path.join(os.getenv("TEST_BIN_DIR"), "bin", "tl")
local tmpdir = TEST_TMPDIR or "/tmp"

TestTealTypeCheck = {}

function TestTealTypeCheck:test_valid_teal_file_passes()
  local test_file = path.join(tmpdir, "test_valid.tl")
  local content = [[
local record Person
  name: string
  age: number
end

local function greet(p: Person): string
  return "Hello, " .. p.name
end

local person: Person = {
  name = "Alice",
  age = 30
}

print(greet(person))
]]

  cosmo.Barf(test_file, content, tonumber("644", 8))

  local handle = spawn({ lua_dist, tl_bin, "check", test_file })
  local exit_code = handle:wait()

  unix.unlink(test_file)
  luaunit.assertEquals(exit_code, 0)
end

function TestTealTypeCheck:test_invalid_teal_file_fails()
  local test_file = path.join(tmpdir, "test_invalid.tl")
  local content = [[
local record Person
  name: string
  age: number
end

local function greet(p: Person): string
  return "Hello, " .. p.name
end

-- Type error: passing string where Person expected
local result: string = greet("Alice")
print(result)
]]

  cosmo.Barf(test_file, content, tonumber("644", 8))

  local handle = spawn({ lua_dist, tl_bin, "check", test_file })
  local exit_code = handle:wait()

  unix.unlink(test_file)
  luaunit.assertNotEquals(exit_code, 0)
end

function TestTealTypeCheck:test_missing_type_annotation_fails()
  local test_file = path.join(tmpdir, "test_missing_type.tl")
  local content = [[
local record Person
  name: string
  age
end

local person: Person = {
  name = "Alice",
  age = 30
}
]]

  cosmo.Barf(test_file, content, tonumber("644", 8))

  local handle = spawn({ lua_dist, tl_bin, "check", test_file })
  local exit_code = handle:wait()

  unix.unlink(test_file)
  luaunit.assertNotEquals(exit_code, 0)
end

os.exit(luaunit.LuaUnit.run())
