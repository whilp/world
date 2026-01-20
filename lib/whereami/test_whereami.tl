local unix = require('cosmo.unix')
local path = require('cosmo.path')
local cosmo = require('cosmo')
local whereami = require('whereami')

local function test_whereami_get()
  local identifier = whereami.get()
  assert(identifier, "whereami.get() should return a value")
  assert(type(identifier) == "string", "whereami.get() should return a string")
  assert(#identifier > 0, "whereami.get() should return a non-empty string")
  assert(identifier ~= "unknown", "whereami.get() should not return 'unknown' in a normal environment")
end
test_whereami_get()

local function test_whereami_get_with_emoji()
  local identifier = whereami.get_with_emoji()
  assert(identifier, "whereami.get_with_emoji() should return a value")
  assert(type(identifier) == "string", "whereami.get_with_emoji() should return a string")
  assert(#identifier > 0, "whereami.get_with_emoji() should return a non-empty string")
  -- Should contain a space (separating identifier and emoji)
  assert(identifier:find(" "),
    "whereami.get_with_emoji() should contain identifier and emoji separated by space")
end
test_whereami_get_with_emoji()

local function test_whereami_codespaces_format()
  local env: {string:string} = { CODESPACES = 'true', GITHUB_REPOSITORY = 'owner/repo' }
  local result = whereami.get_with_emoji(function(k: string): string return env[k] end)
  -- In codespaces mode, result should be "repo | hostname emoji"
  assert(result:match('^repo | .+'), "codespaces result should match 'repo | ...' pattern")
end
test_whereami_codespaces_format()

local function test_whereami_box_name_kind()
  local tmpdir = os.getenv("TEST_TMPDIR")
  if not tmpdir then
    print("skipping test_whereami_box_name_kind (no TEST_TMPDIR)")
    return
  end

  -- Create fake home with .config/box/{name,kind}
  local fake_home = path.join(tmpdir, "home")
  local box_dir = path.join(fake_home, ".config", "box")
  unix.makedirs(box_dir)
  cosmo.Barf(path.join(box_dir, "name"), "mybox\n")
  cosmo.Barf(path.join(box_dir, "kind"), "sprite\n")

  -- Swap HOME, reload module, test, restore
  local orig_home = os.getenv("HOME")
  unix.setenv("HOME", fake_home)

  -- Clear cached module to pick up new HOME
  package.loaded["whereami"] = nil
  local fresh_whereami = require("whereami")

  local result = fresh_whereami.get()
  assert(result == "mybox.sprite", "expected 'mybox.sprite', got '" .. result .. "'")

  local with_emoji = fresh_whereami.get_with_emoji()
  assert(with_emoji:match("^mybox%.sprite ."), "expected 'mybox.sprite <emoji>', got '" .. with_emoji .. "'")

  -- Restore
  unix.setenv("HOME", orig_home)
  package.loaded["whereami"] = nil
end
test_whereami_box_name_kind()

local function test_whereami_generic_backend()
  local tmpdir = os.getenv("TEST_TMPDIR")
  if not tmpdir then
    print("skipping test_whereami_generic_backend (no TEST_TMPDIR)")
    return
  end

  -- Create fake home with .config/box/{name,kind} using generic:backend format
  local fake_home = path.join(tmpdir, "home2")
  local box_dir = path.join(fake_home, ".config", "box")
  unix.makedirs(box_dir)
  cosmo.Barf(path.join(box_dir, "name"), "alpha\n")
  cosmo.Barf(path.join(box_dir, "kind"), "generic:devbox\n")

  local orig_home = os.getenv("HOME")
  unix.setenv("HOME", fake_home)
  package.loaded["whereami"] = nil
  local fresh_whereami = require("whereami")

  local result = fresh_whereami.get()
  assert(result == "alpha.devbox", "expected 'alpha.devbox', got '" .. result .. "'")

  unix.setenv("HOME", orig_home)
  package.loaded["whereami"] = nil
end
test_whereami_generic_backend()
