-- Tests for download-tool module

local lu = require("luaunit")
local download_tool = require("build.download-tool")

-- Test template interpolation
function test_interpolate_replaces_variables()
  local template = "https://example.com/{version}/file-{release_sha}.tar.gz"
  local vars = { version = "1.0.0", release_sha = "abc123" }

  local result = download_tool.interpolate(template, vars)

  lu.assertEquals(result, "https://example.com/1.0.0/file-abc123.tar.gz")
end

function test_interpolate_handles_missing_variables()
  local template = "https://example.com/{version}/file-{missing}.tar.gz"
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

function test_interpolate_handles_custom_variables()
  local template = "https://example.com/{version}/file-{arch}.tar.gz"
  local vars = { version = "1.0.0", arch = "x86_64-unknown-linux-musl" }

  local result = download_tool.interpolate(template, vars)

  lu.assertEquals(result, "https://example.com/1.0.0/file-x86_64-unknown-linux-musl.tar.gz")
end

function test_interpolate_handles_platform_variable()
  local template = "https://example.com/{version}/file-{platform}.tar.gz"
  local vars = { version = "1.0.0", platform = "darwin-arm64" }

  local result = download_tool.interpolate(template, vars)

  lu.assertEquals(result, "https://example.com/1.0.0/file-darwin-arm64.tar.gz")
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

os.exit(lu.LuaUnit.run())
