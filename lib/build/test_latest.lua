local lu = require("luaunit")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local cosmo = require("cosmo")

local latest = dofile("lib/build/latest.lua")

TestExtractGithubRepo = {}

function TestExtractGithubRepo:test_standard_releases_url()
  local url = "https://github.com/owner/repo/releases/download/{version}/file.tar.gz"
  local repo = latest.extract_github_repo(url)
  lu.assertEquals(repo, "owner/repo")
end

function TestExtractGithubRepo:test_with_dash_in_name()
  local url = "https://github.com/some-org/my-tool/releases/download/v{version}/tool-{platform}.zip"
  local repo = latest.extract_github_repo(url)
  lu.assertEquals(repo, "some-org/my-tool")
end

function TestExtractGithubRepo:test_non_github_url()
  local url = "https://storage.googleapis.com/bucket/file.zip"
  local repo = latest.extract_github_repo(url)
  lu.assertNil(repo)
end

function TestExtractGithubRepo:test_raw_githubusercontent()
  local url = "https://raw.githubusercontent.com/user/repo/branch/file.lua"
  local repo = latest.extract_github_repo(url)
  lu.assertNil(repo)
end

TestParseSha256sums = {}

function TestParseSha256sums:test_standard_format()
  local text = [[
abc123def456  file1.tar.gz
789abc456def  file2.zip
111222333444  file3.tar.gz
]]
  local sums = latest.parse_sha256sums(text)
  lu.assertEquals(sums["file1.tar.gz"], "abc123def456")
  lu.assertEquals(sums["file2.zip"], "789abc456def")
  lu.assertEquals(sums["file3.tar.gz"], "111222333444")
end

function TestParseSha256sums:test_with_path()
  local text = [[
abc123  ./dist/file.tar.gz
def456  dist/file2.zip
]]
  local sums = latest.parse_sha256sums(text)
  lu.assertEquals(sums["./dist/file.tar.gz"], "abc123")
  lu.assertEquals(sums["dist/file2.zip"], "def456")
end

function TestParseSha256sums:test_uppercase_hex()
  local text = "ABC123DEF456  FILE.TAR.GZ\n"
  local sums = latest.parse_sha256sums(text)
  lu.assertEquals(sums["FILE.TAR.GZ"], "abc123def456")
end

function TestParseSha256sums:test_empty()
  local sums = latest.parse_sha256sums("")
  lu.assertEquals(next(sums), nil)
end

TestInterpolate = {}

function TestInterpolate:test_single_variable()
  local template = "https://example.com/{version}/file.tar.gz"
  local vars = {version = "1.2.3"}
  local result = latest.interpolate(template, vars)
  lu.assertEquals(result, "https://example.com/1.2.3/file.tar.gz")
end

function TestInterpolate:test_multiple_variables()
  local template = "tool-{version}-{platform}.{ext}"
  local vars = {version = "2.0", platform = "linux-x64", ext = "zip"}
  local result = latest.interpolate(template, vars)
  lu.assertEquals(result, "tool-2.0-linux-x64.zip")
end

function TestInterpolate:test_missing_variable()
  local template = "file-{version}-{missing}.tar.gz"
  local vars = {version = "1.0"}
  local result = latest.interpolate(template, vars)
  lu.assertEquals(result, "file-1.0-{missing}.tar.gz")
end

function TestInterpolate:test_no_variables()
  local template = "https://example.com/static.tar.gz"
  local result = latest.interpolate(template, {})
  lu.assertEquals(result, "https://example.com/static.tar.gz")
end

TestInferAssetName = {}

function TestInferAssetName:test_simple_platform()
  local url_template = "https://github.com/org/repo/releases/download/{version}/tool-{platform}.tar.gz"
  local platform_config = {sha = "abc123"}
  local platform_key = "linux-x86_64"
  local version_info = {version = "1.0", tag = "v1.0"}

  local asset_name, full_url = latest.infer_asset_name(
    url_template, platform_config, platform_key, version_info)

  lu.assertEquals(asset_name, "tool-linux-x86_64.tar.gz")
  lu.assertStrContains(full_url, "1.0")
  lu.assertStrContains(full_url, "linux-x86_64")
end

function TestInferAssetName:test_platform_remap()
  local url_template = "https://github.com/org/repo/releases/download/{version}/tool-{platform}.tar.gz"
  local platform_config = {platform = "macos-arm64", sha = "abc123"}
  local platform_key = "darwin-arm64"
  local version_info = {version = "1.0", tag = "v1.0"}

  local asset_name, full_url = latest.infer_asset_name(
    url_template, platform_config, platform_key, version_info)

  lu.assertEquals(asset_name, "tool-macos-arm64.tar.gz")
end

function TestInferAssetName:test_multiple_variables()
  local url_template = "https://github.com/org/repo/releases/download/v{version}/tool_{version}_{os}_{arch}.{ext}"
  local platform_config = {
    os = "linux",
    arch = "amd64",
    ext = "tar.gz",
    sha = "abc123"
  }
  local platform_key = "linux-x86_64"
  local version_info = {version = "2.0", tag = "v2.0"}

  local asset_name, full_url = latest.infer_asset_name(
    url_template, platform_config, platform_key, version_info)

  lu.assertEquals(asset_name, "tool_2.0_linux_amd64.tar.gz")
end

function TestInferAssetName:test_with_tag()
  local url_template = "https://github.com/org/repo/releases/download/{tag}/nvim-{platform}.tar.gz"
  local platform_config = {sha = "abc123"}
  local platform_key = "linux-x86_64"
  local version_info = {version = "0.12.0-dev", tag = "2025.12.28-abc"}

  local asset_name, full_url = latest.infer_asset_name(
    url_template, platform_config, platform_key, version_info)

  lu.assertEquals(asset_name, "nvim-linux-x86_64.tar.gz")
  lu.assertStrContains(full_url, "2025.12.28-abc")
end

TestIsGithubUrl = {}

function TestIsGithubUrl:test_github_releases()
  local url = "https://github.com/owner/repo/releases/download/{version}/file.tar.gz"
  local repo = latest.extract_github_repo(url)
  lu.assertEquals(repo, "owner/repo")
end

function TestIsGithubUrl:test_non_github()
  local url = "https://storage.googleapis.com/bucket/file.zip"
  local repo = latest.extract_github_repo(url)
  lu.assertNil(repo)
end

function TestIsGithubUrl:test_raw_github()
  local url = "https://raw.githubusercontent.com/user/repo/main/file.lua"
  local repo = latest.extract_github_repo(url)
  lu.assertNil(repo)
end

TestConfigPreservation = {}

function TestConfigPreservation:test_preserves_format()
  local original = {
    version = "1.0",
    format = "tar.gz",
    strip_components = 1,
    url = "https://github.com/org/repo/releases/download/{version}/file.tar.gz",
    platforms = {["*"] = {sha = "abc"}},
  }

  local result = {
    version = "2.0",
    tag = "v2.0",
  }

  for k, v in pairs(original) do
    if k ~= "version" and k ~= "platforms" and k ~= "tag" and k ~= "sha" and type(v) ~= "table" then
      result[k] = v
    end
  end

  lu.assertEquals(result.format, "tar.gz")
  lu.assertEquals(result.strip_components, 1)
  lu.assertEquals(result.url, original.url)
end

TestCheckFunction = {}

function TestCheckFunction:test_non_github_url_gets_todo()
  local content = [[
return {
  version = "1.0",
  url = "https://example.com/file.zip",
  platforms = {["*"] = {sha = "abc123"}},
}
]]

  local opts = {stderr = {write = function() end}}
  local result_str, err = latest.check(content, opts)

  lu.assertNotNil(result_str, "check should return result string")
  lu.assertNil(err)

  local result = load(result_str)()
  lu.assertTrue(result._todo)
  lu.assertEquals(result.version, "1.0")
end

function TestCheckFunction:test_github_url_with_version()
  local content = [[
return {
  version = "1.0.0",
  url = "https://github.com/owner/repo/releases/download/{version}/file.tar.gz",
  platforms = {["*"] = {sha = "abc123"}},
}
]]

  local opts = {stderr = {write = function() end}}
  local result_str, err = latest.check(content, opts)

  lu.assertNotNil(result_str, "check should return result string")

  local result = load(result_str)()
  lu.assertNotNil(result.version)
  lu.assertNotNil(result.platforms)
end

TestReportFunction = {}

function TestReportFunction:test_counts_todo_and_up_to_date()
  unix.makedirs(path.join(TEST_TMPDIR, "3p/tool1"))
  unix.makedirs(path.join(TEST_TMPDIR, "3p/tool2"))
  unix.makedirs(path.join(TEST_TMPDIR, "lib/tool3"))

  cosmo.Barf(path.join(TEST_TMPDIR, "3p/tool1/version.lua.latest.ok"),
    "return {version='1.0', _todo=true, platforms={['*']={sha='abc'}}}")

  cosmo.Barf(path.join(TEST_TMPDIR, "3p/tool2/version.lua.latest.ok"),
    "return {version='2.0', platforms={['*']={sha='def'}}}")

  cosmo.Barf(path.join(TEST_TMPDIR, "lib/tool3/version.lua.latest.ok"),
    "return {version='3.0', platforms={['*']={sha='ghi'}}}")

  local old_print = print
  local output = {}
  print = function(s) table.insert(output, s) end

  local ok = latest.report(TEST_TMPDIR)

  print = old_print

  lu.assertTrue(ok)
  local summary = table.concat(output, "\n")
  lu.assertStrContains(summary, "3 total")
  lu.assertStrContains(summary, "2 up-to-date")
  lu.assertStrContains(summary, "1 need work")
end

TestIntegration = {}

function TestIntegration:test_cosmos_real_fetch()
  lu.skipIf(not TEST_INTEGRATION, "Integration test - set TEST_INTEGRATION=1 to run")

  local config = {
    version = "2025.12.31-b0b834275",
    format = "zip",
    url = "https://github.com/whilp/cosmopolitan/releases/download/{version}/cosmos.zip",
    platforms = {["*"] = {sha = "994a45a2a9c9e611a8edc936274f4e418a38643f688e304ae160e39609db3e2f"}},
  }

  local result, err = latest.fetch_latest_github(config, {stderr = io.stderr})
  lu.assertNotNil(result, "Failed to fetch: " .. tostring(err))

  lu.assertNotNil(result.version)
  lu.assertNotNil(result.platforms["*"])
  lu.assertNotNil(result.platforms["*"].sha)
  lu.assertEquals(#result.platforms["*"].sha, 64)
end

function TestIntegration:test_nvim_real_fetch()
  lu.skipIf(not TEST_INTEGRATION, "Integration test - set TEST_INTEGRATION=1 to run")

  local config = {
    version = "0.12.0-dev",
    format = "tar.gz",
    strip_components = 1,
    url = "https://github.com/whilp/neovim/releases/download/{tag}/nvim-{platform}.tar.gz",
    platforms = {
      ["darwin-arm64"] = {platform = "macos-arm64", sha = "abc"},
      ["linux-x86_64"] = {sha = "def"},
    },
  }

  local result, err = latest.fetch_latest_github(config, {stderr = io.stderr})
  lu.assertNotNil(result, "Failed to fetch: " .. tostring(err))

  lu.assertNotNil(result.version)
  lu.assertNotNil(result.tag)
  lu.assertNotNil(result.platforms["darwin-arm64"])
  lu.assertNotNil(result.platforms["linux-x86_64"])
  lu.assertEquals(result.platforms["darwin-arm64"].platform, "macos-arm64")
  lu.assertEquals(#result.platforms["darwin-arm64"].sha, 64)
end

TestEdgeCases = {}

function TestEdgeCases:test_parse_empty_sha256sums()
  local text = ""
  local result = latest.parse_sha256sums(text)
  lu.assertEquals(next(result), nil, "should return empty table")
end

function TestEdgeCases:test_parse_malformed_sha256sums()
  local text = "not a valid checksum line\n"
  local result = latest.parse_sha256sums(text)
  lu.assertEquals(next(result), nil, "should return empty table for malformed input")
end

function TestEdgeCases:test_interpolate_missing_variable()
  local template = "https://example.com/{missing}/file.tar.gz"
  local vars = {version = "1.0"}
  local result = latest.interpolate(template, vars)
  lu.assertStrContains(result, "{missing}", "should preserve unmatched variables")
end

TestUnsupportedCases = {}

function TestUnsupportedCases:test_url_encoded_tags()
  lu.skipIf(not TEST_UNSUPPORTED, "URL-encoded tags not yet supported (e.g., cli%%2Fv1.0)")
  lu.fail("Test implementation needed when feature is supported")
end

function TestUnsupportedCases:test_non_github_storage()
  lu.skipIf(not TEST_UNSUPPORTED, "Non-GitHub storage (e.g., storage.googleapis.com) not yet supported")
  lu.fail("Test implementation needed when feature is supported")
end

function TestUnsupportedCases:test_raw_githubusercontent()
  lu.skipIf(not TEST_UNSUPPORTED, "raw.githubusercontent.com (non-release) not yet supported")
  lu.fail("Test implementation needed when feature is supported")
end

function TestUnsupportedCases:test_missing_platform_assets()
  lu.skipIf(not TEST_UNSUPPORTED, "Handling missing platform assets (404) - falls back to _todo")
  lu.fail("Test implementation needed when feature is supported")
end

