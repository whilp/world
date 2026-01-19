-- type declarations for claude.version

local record ClaudePlatform
  sha: string
end

local record ClaudeVersion
  base_url: string
  platforms: {string: ClaudePlatform}
  url: string
  version: string
end

return ClaudeVersion
