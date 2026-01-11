-- type declarations for whereami module

local type EnvFunc = function(string): string

local record whereami
  get: function(): string
  get_with_emoji: function(env?: EnvFunc): string
end

return whereami
