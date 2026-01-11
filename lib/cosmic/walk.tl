-- cosmic.walk: directory tree walking utilities
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local record Stat
  mode: function(self): number
  size: function(self): number
  mtim: function(self): number
end

local record DirHandle
  read: function(self): string
  close: function(self)
end

local record FileInfo
  mode: number
end

local type Visitor = function(full_path: string, entry: string, stat: Stat, ctx: any): boolean

local function walk<T>(dir: string, visitor: Visitor, ctx?: T): T
  ctx = ctx or {} as T
  local handle = unix.opendir(dir) as DirHandle
  if not handle then return ctx end

  while true do
    local entry = handle:read()
    if not entry then break end
    if entry ~= "." and entry ~= ".." then
      local full_path = path.join(dir, entry)
      local stat = unix.stat(full_path) as Stat
      if stat then
        local continue = visitor(full_path, entry, stat, ctx)
        if continue ~= false and unix.S_ISDIR(stat:mode()) then
          walk(full_path, visitor, ctx)
        end
      end
    end
  end
  handle:close()
  return ctx
end

local function collect(dir: string, pattern: string): {string}
  local results: {string} = {}
  walk(dir, function(full_path: string, entry: string, stat: Stat, ctx: {string}): boolean
    if not unix.S_ISDIR(stat:mode()) and entry:match(pattern) then
      table.insert(ctx, full_path)
    end
    return true
  end, results)
  return results
end

local function collect_all(dir: string, base?: string, files?: {string:FileInfo}): {string:FileInfo}
  files = files or {}
  base = base or ""

  local handle = unix.opendir(dir) as DirHandle
  if not handle then return files end

  while true do
    local entry = handle:read()
    if not entry then break end
    if entry ~= "." and entry ~= ".." then
      local full_path = path.join(dir, entry)
      local rel_path = base == "" and entry or path.join(base, entry)
      local stat = unix.stat(full_path) as Stat

      if stat then
        if unix.S_ISDIR(stat:mode()) then
          collect_all(full_path, rel_path, files)
        elseif unix.S_ISREG(stat:mode()) or unix.S_ISLNK(stat:mode()) then
          local mode = stat:mode() & 0x1ff
          files[rel_path] = { mode = mode }
        end
      end
    end
  end
  handle:close()
  return files
end

return {
  walk = walk,
  collect = collect,
  collect_all = collect_all,
}
