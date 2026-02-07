-- type declarations for cosmo (cosmopolitan lua)

local record cosmo
  -- http
  Fetch: function(url: string, opts?: {string:any}): number, {string:string}, string

  -- file i/o
  Barf: function(path: string, data: string, mode?: number): boolean
  Slurp: function(path: string): string

  -- hashing
  Sha256: function(data: string): string

  -- encoding
  EncodeHex: function(data: string): string
  EncodeJson: function(value: any): string
  DecodeJson: function(json: string): any
  EncodeLua: function(value: any, opts?: {string:any}): string

  -- system info
  GetHostOs: function(): string
  GetHostIsa: function(): string
  GetRandomBytes: function(count: number): string

  -- script detection
  is_main: function(): boolean

  -- submodules (for require path hints)
  record unix end
  record path end
  record getopt end
  record zip end
  record lsqlite3 end
end

return cosmo
