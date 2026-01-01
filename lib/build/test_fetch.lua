local lu = require("luaunit")

-- reproduce the interpolation logic from fetch.lua
local function interpolate(template, vars)
  return template:gsub("{([%w_]+)}", function(key)
    return tostring(vars[key] or "")
  end)
end

-- reproduce the vars construction logic from fetch.lua
local function build_url(spec, platform)
  local plat = spec.platforms[platform] or spec.platforms["*"]
  if not plat then
    return nil, "unknown platform: " .. platform
  end

  local vars = {platform = platform}
  for k, v in pairs(spec) do
    if type(v) ~= "table" then
      vars[k] = v
    end
  end
  for k, v in pairs(plat) do
    vars[k] = v
  end

  return interpolate(spec.url, vars)
end

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

  local url = build_url(spec, "linux-x86_64")
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

  local url = build_url(spec, "darwin-arm64")
  lu.assertEquals(url, "https://example.com/1.0/tool-darwin-arm64")
end
