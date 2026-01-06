#!/usr/bin/env run-test.lua
-- teal ignore: test file

local version = require("version")

-- Test parse_version_dir with valid inputs
local function test_parse_valid()
  -- Standard full sha256
  local v, s = version.parse_version_dir("1.2.3-abc123def456")
  assert(v == "1.2.3", "version should be 1.2.3, got: " .. tostring(v))
  assert(s == "abc123def456", "sha should be abc123def456, got: " .. tostring(s))

  -- Full 64-char sha256
  local v2, s2 = version.parse_version_dir("2.0.0-e7af0c72a607c0528fda1989f7c8e3be85e67d321889002af0e2938ad9c8fb68")
  assert(v2 == "2.0.0", "version should be 2.0.0")
  assert(s2 == "e7af0c72a607c0528fda1989f7c8e3be85e67d321889002af0e2938ad9c8fb68", "sha should be full 64 chars")

  -- Minimum 6-char sha
  local v3, s3 = version.parse_version_dir("1.0.0-abc123")
  assert(v3 == "1.0.0", "version should be 1.0.0")
  assert(s3 == "abc123", "sha should be abc123")

  -- Complex version with dev/pre-release
  local v4, s4 = version.parse_version_dir("0.12.0-dev-49450c182c-91a8b837")
  assert(v4 == "0.12.0-dev-49450c182c", "version should preserve dev part")
  assert(s4 == "91a8b837", "sha should be extracted correctly")

  -- Version with multiple dashes
  local v5, s5 = version.parse_version_dir("2024-12-18-b9cb666")
  assert(v5 == "2024-12-18", "version should be 2024-12-18")
  assert(s5 == "b9cb666", "sha should be b9cb666")
end
test_parse_valid()

-- Test parse_version_dir with invalid inputs
local function test_parse_invalid()
  -- Too short sha (less than 6 chars)
  local v1, s1 = version.parse_version_dir("1.0.0-abc12")
  assert(v1 == nil, "should reject sha with 5 chars")
  assert(s1 == nil, "sha should be nil")

  -- No dash
  local v2, s2 = version.parse_version_dir("1.0.0")
  assert(v2 == nil, "should reject version without dash")
  assert(s2 == nil, "sha should be nil")

  -- Empty version
  local v3, s3 = version.parse_version_dir("-abc123")
  assert(v3 == nil, "should reject empty version")
  assert(s3 == nil, "sha should be nil")

  -- Non-hex characters in sha
  local v4, s4 = version.parse_version_dir("1.0.0-ghijkl")
  assert(v4 == nil, "should reject non-hex sha")
  assert(s4 == nil, "sha should be nil")

  -- Empty string
  local v5, s5 = version.parse_version_dir("")
  assert(v5 == nil, "should reject empty string")
  assert(s5 == nil, "sha should be nil")

  -- Just a dash
  local v6, s6 = version.parse_version_dir("-")
  assert(v6 == nil, "should reject just dash")
  assert(s6 == nil, "sha should be nil")

  -- Sha-like version with non-hex sha
  local v7, s7 = version.parse_version_dir("abc123-1.0.0")
  assert(v7 == nil, "should reject version part with non-hex in sha position")
  assert(s7 == nil, "sha should be nil")
end
test_parse_invalid()

-- Test is_version_dir
local function test_is_version_dir()
  -- Valid patterns
  assert(version.is_version_dir("1.2.3-abc123"), "should accept standard version-sha")
  assert(version.is_version_dir("2.79.0-e7af0c7"), "should accept real gh version")
  assert(version.is_version_dir("0.12.0-dev-49450c182c-91a8b83"), "should accept nvim version")
  assert(version.is_version_dir("2024-12-18-b9cb666"), "should accept date-based version")

  -- Invalid patterns
  assert(not version.is_version_dir("1.0.0"), "should reject version without sha")
  assert(not version.is_version_dir("bin"), "should reject non-versioned dir")
  assert(not version.is_version_dir("share"), "should reject non-versioned dir")
  assert(not version.is_version_dir("LICENSE"), "should reject non-versioned dir")
  assert(not version.is_version_dir("1.0.0-abc"), "should reject short sha")
  assert(not version.is_version_dir("-abc123"), "should reject empty version")
  assert(not version.is_version_dir(""), "should reject empty string")
  assert(not version.is_version_dir("."), "should reject dot")
  assert(not version.is_version_dir(".."), "should reject dotdot")
end
test_is_version_dir()

-- Test with real-world examples
local function test_real_world()
  -- From actual home binary
  local real_versions = {
    "2.79.0-e7af0c72a607c0528fda1989f7c8e3be85e67d321889002af0e2938ad9c8fb68",  -- gh
    "0.12.0-dev-49450c182c-91a8b837a7e3665b5bd2a5b9419a5619262cdb8450f2707aa5febccfd81d9907",  -- nvim
    "0.18.2-b7ea845004762358a00ef9127dd9fd723e333c7e4b9cb1da220c3909372310ee",  -- delta
    "1.4.2-fae3ba93eedf20b08bca4b23aeac1ba94c446f1c10d029c193e2fc4b4e0bc1bc",  -- duckdb
    "2024-12-18-b9cb666a4f360cba7b80c0f016c0e0d4af3f1a6b6e23bee85a0ff2b5da96b49",  -- marksman
  }

  for _, name in ipairs(real_versions) do
    local v, s = version.parse_version_dir(name)
    assert(v ~= nil, "should parse real version: " .. name)
    assert(s ~= nil, "should extract sha from: " .. name)
    assert(#s >= 6, "sha should be at least 6 chars: " .. s)
    assert(version.is_version_dir(name), "is_version_dir should accept: " .. name)
  end

  -- Non-versioned directories that should be rejected
  local non_versions = {"bin", "share", "lib", "LICENSE", "README.md", ".", ".."}
  for _, name in ipairs(non_versions) do
    assert(not version.is_version_dir(name), "should reject non-version: " .. name)
  end
end
test_real_world()
