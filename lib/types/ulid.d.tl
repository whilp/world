-- type declarations for ulid

local record DecodedUlid
  timestamp: number
  time: string
  random: string
end

local record ulid
  generate: function(): string
  timestamp: function(ulid_str: string): number, string
  decode: function(ulid_str: string): DecodedUlid, string
end

return ulid
