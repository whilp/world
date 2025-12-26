-- test whereami module
local cosmo = require('cosmo')
local path = cosmo.path

local whereami = require('whereami')

function test_whereami_get()
  local identifier = whereami.get()
  lu.assertNotNil(identifier, "whereami.get() should return a value")
  lu.assertIsString(identifier, "whereami.get() should return a string")
  lu.assertTrue(#identifier > 0, "whereami.get() should return a non-empty string")
  lu.assertNotEquals(identifier, "unknown", "whereami.get() should not return 'unknown' in a normal environment")
end

function test_whereami_get_with_emoji()
  local identifier = whereami.get_with_emoji()
  lu.assertNotNil(identifier, "whereami.get_with_emoji() should return a value")
  lu.assertIsString(identifier, "whereami.get_with_emoji() should return a string")
  lu.assertTrue(#identifier > 0, "whereami.get_with_emoji() should return a non-empty string")
  -- Should contain a space (separating identifier and emoji)
  lu.assertNotNil(identifier:find(" "),
    "whereami.get_with_emoji() should contain identifier and emoji separated by space")
end

function test_whereami_codespaces_format()
  local original_getenv = os.getenv
  local mock_env = {
    CODESPACES = 'true',
    GITHUB_REPOSITORY = 'testowner/testrepo',
  }
  os.getenv = function(key)
    if mock_env[key] ~= nil then
      return mock_env[key]
    end
    return original_getenv(key)
  end

  package.loaded['whereami'] = nil
  local w = require('whereami')

  local identifier = w.get_with_emoji()

  os.getenv = original_getenv
  package.loaded['whereami'] = nil
  require('whereami')

  lu.assertNotNil(identifier:find(' | '),
    "codespaces format should contain ' | ' separator")

  local repo_branch, host_emoji = identifier:match('(.+) | (.+)')
  lu.assertNotNil(repo_branch, "should have repo/branch before separator")
  lu.assertNotNil(host_emoji, "should have hostname and emoji after separator")

  lu.assertNotNil(repo_branch:find('/'),
    "repo/branch portion should contain a slash")
  lu.assertEquals(repo_branch:match('^[^/]+'), 'testrepo',
    "repo name should be stripped of owner prefix")

  lu.assertNotNil(host_emoji:find(' '),
    "hostname and emoji should be space-separated")
end
