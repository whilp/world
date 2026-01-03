local lu = require("luaunit")
local fetch = require("build.fetch")

TestBuildUrl = {}

-- platform with explicit override should use the override
function TestBuildUrl:test_platform_override()
  local spec = {
    version = "1.0",
    url = "https://example.com/{version}/tool-{platform}",
    platforms = {
      ["linux-x86_64"] = {
        platform = "linux-x64",
        sha = "abc123",
      },
    },
  }

  local url = fetch.build_url(spec, "linux-x86_64")
  lu.assertEquals(url, "https://example.com/1.0/tool-linux-x64")
end

-- platform without override should use the platform key itself
function TestBuildUrl:test_platform_no_override()
  local spec = {
    version = "1.0",
    url = "https://example.com/{version}/tool-{platform}",
    platforms = {
      ["darwin-arm64"] = {
        sha = "abc123",
      },
    },
  }

  local url = fetch.build_url(spec, "darwin-arm64")
  lu.assertEquals(url, "https://example.com/1.0/tool-darwin-arm64")
end
