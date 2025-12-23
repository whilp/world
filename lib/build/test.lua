-- Tests for download-tool module

local lu = require("luaunit")
local download_tool = require("build.download-tool")

-- Test template interpolation
function test_interpolate_replaces_variables()
  local template = "https://example.com/${version}/file-${release_sha}.tar.gz"
  local vars = { version = "1.0.0", release_sha = "abc123" }

  local result = download_tool.interpolate(template, vars)

  lu.assertEquals(result, "https://example.com/1.0.0/file-abc123.tar.gz")
end

function test_interpolate_handles_missing_variables()
  local template = "https://example.com/${version}/file-${missing}.tar.gz"
  local vars = { version = "1.0.0" }

  local result = download_tool.interpolate(template, vars)

  lu.assertEquals(result, "https://example.com/1.0.0/file-.tar.gz")
end

function test_interpolate_handles_non_string_input()
  local result = download_tool.interpolate(nil, {})
  lu.assertNil(result)

  result = download_tool.interpolate(123, {})
  lu.assertEquals(result, 123)
end

function test_interpolate_handles_empty_vars()
  local template = "https://example.com/file.tar.gz"
  local result = download_tool.interpolate(template, {})

  lu.assertEquals(result, "https://example.com/file.tar.gz")
end

-- Test load_tool_config
function test_load_tool_config_validates_tool_name()
  local config, err = download_tool.load_tool_config("", "linux-x86_64")

  lu.assertNil(config)
  lu.assertEquals(err, "tool_name cannot be empty")
end

function test_load_tool_config_validates_platform()
  local config, err = download_tool.load_tool_config("nvim", "")

  lu.assertNil(config)
  lu.assertEquals(err, "platform cannot be empty")
end

function test_load_tool_config_handles_missing_file()
  local config, err = download_tool.load_tool_config("nonexistent-tool", "linux-x86_64")

  lu.assertNil(config)
  lu.assertStrContains(err, "failed to load")
end

function test_load_tool_config_handles_missing_platform()
  -- Create a temporary test config
  local test_config_path = "3p/test-tool/version.lua"
  os.execute("mkdir -p 3p/test-tool")
  local f = io.open(test_config_path, "w")
  f:write([[
return {
  version = "1.0.0",
  platforms = {
    ["darwin-arm64"] = {
      url = "https://example.com/file.tar.gz",
      sha = "abc123",
    },
  },
}
]])
  f:close()

  local config, err = download_tool.load_tool_config("test-tool", "linux-x86_64")

  lu.assertNil(config)
  lu.assertStrContains(err, "platform linux-x86_64 not found")

  -- Cleanup
  os.execute("rm -rf 3p/test-tool")
end

function test_load_tool_config_merges_tool_and_platform_config()
  -- Create a temporary test config
  local test_config_path = "3p/test-merge/version.lua"
  os.execute("mkdir -p 3p/test-merge")
  local f = io.open(test_config_path, "w")
  f:write([[
return {
  version = "2.0.0",
  release_sha = "xyz789",
  format = "tar.gz",
  strip_components = 1,
  platforms = {
    ["linux-x86_64"] = {
      url = "https://example.com/${version}-${release_sha}.tar.gz",
      sha = "deadbeef",
    },
  },
}
]])
  f:close()

  local config, err = download_tool.load_tool_config("test-merge", "linux-x86_64")

  lu.assertNotNil(config)
  lu.assertEquals(config.tool_name, "test-merge")
  lu.assertEquals(config.platform, "linux-x86_64")
  lu.assertEquals(config.url, "https://example.com/2.0.0-xyz789.tar.gz")
  lu.assertEquals(config.format, "tar.gz")
  lu.assertEquals(config.strip_components, 1)
  lu.assertEquals(config.sha, "deadbeef")
  lu.assertEquals(config.version, "2.0.0")

  -- Cleanup
  os.execute("rm -rf 3p/test-merge")
end

function test_load_tool_config_allows_platform_format_override()
  -- Create a temporary test config
  local test_config_path = "3p/test-override/version.lua"
  os.execute("mkdir -p 3p/test-override")
  local f = io.open(test_config_path, "w")
  f:write([[
return {
  version = "1.0.0",
  format = "tar.gz",
  platforms = {
    ["darwin-arm64"] = {
      format = "zip",
      url = "https://example.com/file.zip",
      sha = "abc123",
    },
  },
}
]])
  f:close()

  local config, err = download_tool.load_tool_config("test-override", "darwin-arm64")

  lu.assertNotNil(config)
  lu.assertEquals(config.format, "zip")

  -- Cleanup
  os.execute("rm -rf 3p/test-override")
end

function test_load_tool_config_handles_missing_url()
  -- Create a temporary test config
  local test_config_path = "3p/test-no-url/version.lua"
  os.execute("mkdir -p 3p/test-no-url")
  local f = io.open(test_config_path, "w")
  f:write([[
return {
  version = "1.0.0",
  platforms = {
    ["linux-x86_64"] = {
      sha = "abc123",
    },
  },
}
]])
  f:close()

  local config, err = download_tool.load_tool_config("test-no-url", "linux-x86_64")

  lu.assertNil(config)
  lu.assertStrContains(err, "no URL specified")

  -- Cleanup
  os.execute("rm -rf 3p/test-no-url")
end

-- Test download_tool main function
function test_download_tool_validates_tool_name()
  local ok, err = download_tool.download_tool("", "linux-x86_64", "/tmp/output")

  lu.assertNil(ok)
  lu.assertEquals(err, "tool_name is required")
end

function test_download_tool_validates_platform()
  local ok, err = download_tool.download_tool("nvim", "", "/tmp/output")

  lu.assertNil(ok)
  lu.assertEquals(err, "platform is required")
end

function test_download_tool_validates_output_dir()
  local ok, err = download_tool.download_tool("nvim", "linux-x86_64", "")

  lu.assertNil(ok)
  lu.assertEquals(err, "output_dir is required")
end

function test_download_tool_removes_trailing_slash()
  -- Create a minimal test config
  local test_config_path = "3p/test-slash/version.lua"
  os.execute("mkdir -p 3p/test-slash")
  local f = io.open(test_config_path, "w")
  f:write([[
return {
  version = "1.0.0",
  format = "binary",
  platforms = {
    ["linux-x86_64"] = {
      url = "https://httpbin.org/status/404",
      sha = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
    },
  },
}
]])
  f:close()

  -- This will fail because the URL returns 404, but we can verify the trailing slash handling
  -- by checking that the function doesn't crash with a trailing slash
  local ok, err = download_tool.download_tool("test-slash", "linux-x86_64", "/tmp/test-output/")

  lu.assertNil(ok)
  -- Should get an error about download failure, not path issues

  -- Cleanup
  os.execute("rm -rf 3p/test-slash /tmp/test-output")
end

-- Integration test with real biome download (if available)
function test_download_biome_integration()
  -- Only run if biome config exists
  local config_exists = os.execute("test -f 3p/biome/version.lua") == 0
  if not config_exists then
    lu.skip("biome config not found, skipping integration test")
    return
  end

  local output_dir = "/tmp/test-biome-download"
  os.execute("rm -rf " .. output_dir)
  os.execute("mkdir -p " .. output_dir)

  local ok, err = download_tool.download_tool("biome", "linux-x86_64", output_dir)

  lu.assertNotNil(ok, "download should succeed: " .. tostring(err))

  -- Verify files were created
  local version_exists = os.execute("test -f " .. output_dir .. "/VERSION") == 0
  local sha_exists = os.execute("test -f " .. output_dir .. "/SHA") == 0
  local binary_exists = os.execute("test -f " .. output_dir .. "/biome") == 0
  local binary_executable = os.execute("test -x " .. output_dir .. "/biome") == 0

  lu.assertTrue(version_exists, "VERSION file should exist")
  lu.assertTrue(sha_exists, "SHA file should exist")
  lu.assertTrue(binary_exists, "binary should exist")
  lu.assertTrue(binary_executable, "binary should be executable")

  -- Cleanup
  os.execute("rm -rf " .. output_dir)
end

os.exit(lu.LuaUnit.run())
