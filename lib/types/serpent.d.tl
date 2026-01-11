-- type declarations for serpent

local record SerpentOpts
  comment: boolean
  sortkeys: boolean
  indent: string
  sparse: boolean
  compact: boolean
  fatal: boolean
  nocode: boolean
  nohuge: boolean
  maxlevel: number
  maxnum: number
  maxlen: number
  metatostring: boolean
  numformat: string
  valignore: {any:boolean}
  keyallow: {any:boolean}
  keyignore: {any:boolean}
  valtypeignore: {string:boolean}
  custom: function(tag: string, head: string, body: string, tail: string): string
  name: string
end

local record serpent
  dump: function(data: any, opts?: SerpentOpts): string
  line: function(data: any, opts?: SerpentOpts): string
  block: function(data: any, opts?: SerpentOpts): string
  load: function(str: string, opts?: {string:boolean}): any, string
end

return serpent
