-- Tests for fetch module

local lu = require("luaunit")
local fetch = require("build.fetch")

-- Test template interpolation
function test_interpolate_replaces_variables()
  local template = "https://example.com/{version}/file-{arch}.tar.gz"
  local vars = {version = "1.0.0", arch = "abc123"}

  local result = fetch.interpolate(template, vars)

  lu.assertEquals(result, "https://example.com/1.0.0/file-abc123.tar.gz")
end

function test_interpolate_handles_missing_variables()
  local template = "https://example.com/{version}/file-{missing}.tar.gz"
  local vars = {version = "1.0.0"}

  local result = fetch.interpolate(template, vars)

  lu.assertEquals(result, "https://example.com/1.0.0/file-.tar.gz")
end

function test_interpolate_handles_non_string_input()
  local result = fetch.interpolate(nil, {})
  lu.assertNil(result)

  result = fetch.interpolate(123, {})
  lu.assertEquals(result, 123)
end

function test_interpolate_handles_empty_vars()
  local template = "https://example.com/file.tar.gz"
  local result = fetch.interpolate(template, {})

  lu.assertEquals(result, "https://example.com/file.tar.gz")
end

-- Test build_config with binaries format (cosmos-style)
function test_build_config_binaries_format()
  local version_data = {
    version = "1.0.0",
    url = "https://example.com/{version}/{binary}",
    binaries = {
      lua = "abc123",
      zip = "def456",
    },
  }

  local config, err = fetch.build_config(version_data, "lua", nil)

  lu.assertNil(err)
  lu.assertEquals(config.url, "https://example.com/1.0.0/lua")
  lu.assertEquals(config.sha, "abc123")
  lu.assertEquals(config.format, "binary")
end

-- Test build_config with platforms format
function test_build_config_platforms_format()
  local version_data = {
    version = "1.0.0",
    format = "tar.gz",
    url = "https://example.com/{version}/tool-{arch}.tar.gz",
    platforms = {
      ["linux-x86_64"] = {
        arch = "x86_64-linux",
        sha = "abc123",
      },
    },
  }

  local config, err = fetch.build_config(version_data, "tool", "linux-x86_64")

  lu.assertNil(err)
  lu.assertEquals(config.url, "https://example.com/1.0.0/tool-x86_64-linux.tar.gz")
  lu.assertEquals(config.sha, "abc123")
  lu.assertEquals(config.format, "tar.gz")
end

function test_build_config_missing_platform()
  local version_data = {
    version = "1.0.0",
    url = "https://example.com/{version}/tool.tar.gz",
    platforms = {
      ["linux-x86_64"] = {sha = "abc123"},
    },
  }

  local config, err = fetch.build_config(version_data, "tool", "darwin-arm64")

  lu.assertNil(config)
  lu.assertStrContains(err, "platform darwin-arm64 not found")
end

function test_build_config_missing_sha()
  local version_data = {
    version = "1.0.0",
    url = "https://example.com/{version}/{binary}",
    binaries = {
      lua = "abc123",
    },
  }

  local config, err = fetch.build_config(version_data, "missing", nil)

  lu.assertNil(config)
  lu.assertStrContains(err, "no sha found")
end

-- Test load_version
function test_load_version_validates_path()
  local data, err = fetch.load_version("")

  lu.assertNil(data)
  lu.assertEquals(err, "version_path cannot be empty")
end

function test_load_version_handles_missing_file()
  local data, err = fetch.load_version("nonexistent.lua")

  lu.assertNil(data)
  lu.assertStrContains(err, "failed to load")
end

os.exit(lu.LuaUnit.run())
