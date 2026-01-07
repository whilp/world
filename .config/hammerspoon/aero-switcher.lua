-- luacheck ignore: hammerspoon runtime
-- Native switcher using aerospace directly with MRU tracking
local M = {}

local chooserStyle = require("chooser-style")
local chooser = nil
local cachedItems = {}
local cachedApps = nil

local AEROSPACE = "/opt/homebrew/bin/aerospace"
local HOME = os.getenv("HOME")
local DB_PATH = HOME .. "/.cache/switch/mru.db"
local MRU_SIZE = 50

local FILTERED_APPS = {
  ["Chess"] = true,
  ["Karabiner-VirtualHIDDevice-Manager"] = true,
  ["Bigmac"] = true,
  ["Books"] = true,
  ["Archive Utility"] = true,
  ["Font Book"] = true,
  ["VoiceOver Utility"] = true,
  ["Stickies"] = true,
  ["TextEdit"] = true,
}

local APP_ADJUSTMENTS = {
  ["Calendar"] = -150,
  ["Calculator"] = -150,
  ["Dictionary"] = -150,
  ["Activity Monitor"] = -150,
}

-- MRU database
local function openDb()
  hs.fs.mkdir(HOME .. "/.cache/switch")
  local db = hs.sqlite3.open(DB_PATH)
  db:exec([[
    create table if not exists mru (
      id integer primary key autoincrement,
      key text unique not null,
      type text not null,
      accessed_at text default current_timestamp
    );
    create index if not exists idx_mru_accessed on mru(accessed_at desc);
  ]])
  return db
end

local function getMruRankings()
  local db = openDb()
  local rankings = {}
  local rank = 1
  for row in db:nrows(string.format("select key from mru order by accessed_at desc limit %d", MRU_SIZE)) do
    rankings[row.key] = rank
    rank = rank + 1
  end
  db:close()
  return rankings
end

local function recordAccess(key, itemType)
  local db = openDb()
  local stmt = db:prepare([[
    insert into mru (key, type) values (?, ?)
    on conflict(key) do update set type = excluded.type, accessed_at = current_timestamp
  ]])
  stmt:bind_values(key, itemType)
  stmt:step()
  stmt:finalize()
  db:exec(string.format("delete from mru where id not in (select id from mru order by accessed_at desc limit %d)", MRU_SIZE))
  db:close()
end

-- Installed apps (cached in memory)
local function scanInstalledApps()
  local apps = {}
  local seen = {}
  local dirs = {"/Applications", "/System/Applications", HOME .. "/Applications"}
  for _, dir in ipairs(dirs) do
    local iter, data = hs.fs.dir(dir)
    if iter then
      for file in iter, data do
        if file:match("%.app$") then
          local name = file:gsub("%.app$", "")
          if not seen[name] and not FILTERED_APPS[name] then
            seen[name] = true
            table.insert(apps, name)
          end
        end
      end
    end
  end
  table.sort(apps)
  return apps
end

local function getInstalledApps()
  if not cachedApps then
    cachedApps = scanInstalledApps()
  end
  return cachedApps
end

-- Scoring
local function computeScore(item, mruRankings)
  local score = 0
  local typePriority = item.type == "window" and 10 or 2
  score = score + (typePriority * 15)

  local mruRank = mruRankings[item.key]
  if mruRank then
    score = score + math.max(0, 100 - (mruRank * 2))
    item.mruRank = mruRank
  end

  local adj = APP_ADJUSTMENTS[item.appName] or 0
  score = score + adj
  return score
end

local function getWindows(callback)
  hs.task.new(AEROSPACE, function(code, stdout, stderr)
    if code ~= 0 then
      callback({})
      return
    end

    local items = {}
    for line in stdout:gmatch("[^\n]+") do
      local window_id, _, app_name, title = line:match("^(%d+)\t([^\t]*)\t([^\t]*)\t(.*)")
      if window_id and not FILTERED_APPS[app_name] then
        local display_title = (title and title ~= "") and title or app_name
        local sub_text = (title and title ~= "") and app_name or "Application window"

        if app_name == "Google Chrome" then
          local profile = title:match(" %- Google Chrome %- (.+)$")
          if profile then
            display_title = title:gsub(" %- Google Chrome %- .+$", "")
            sub_text = app_name .. " - " .. profile
          end
        end

        table.insert(items, {
          text = display_title,
          subText = sub_text,
          key = "window:" .. window_id,
          window_id = tonumber(window_id),
          appName = app_name,
          type = "window",
        })
      end
    end
    callback(items)
  end, {"list-windows", "--all", "--format", "%{window-id}\t%{app-bundle-id}\t%{app-name}\t%{window-title}"}):start()
end

local function refreshItems(callback)
  getWindows(function(windows)
    local mruRankings = getMruRankings()
    local seenApps = {}

    -- Score windows
    for _, item in ipairs(windows) do
      item.score = computeScore(item, mruRankings)
      if item.appName then seenApps[item.appName] = true end
    end

    -- Add installed apps not already showing as windows
    local allItems = {}
    for _, item in ipairs(windows) do
      table.insert(allItems, item)
    end

    for _, appName in ipairs(getInstalledApps()) do
      if not seenApps[appName] then
        local item = {
          text = appName,
          subText = "Launch application",
          key = "app:" .. appName,
          appName = appName,
          type = "installed_app",
        }
        item.score = computeScore(item, mruRankings)
        table.insert(allItems, item)
      end
    end

    -- Sort by score
    table.sort(allItems, function(a, b) return a.score > b.score end)

    -- Move MRU rank 1 to second position
    if allItems[1] and allItems[1].mruRank == 1 and allItems[2] then
      local current = table.remove(allItems, 1)
      table.insert(allItems, 2, current)
    end

    cachedItems = allItems
    if callback then callback() end
  end)
end

local function onSelect(choice)
  if not choice or not choice.key then return end

  -- Defer everything so chooser closes immediately
  hs.timer.doAfter(0, function()
    local item_type, id = choice.key:match("^(%w+):(.+)$")
    if item_type == "window" then
      hs.task.new(AEROSPACE, nil, {"focus", "--window-id", id}):start()
      recordAccess(choice.key, "window")
    elseif item_type == "app" then
      hs.task.new("/usr/bin/open", nil, {"-a", id}):start()
      recordAccess(choice.key, "app")
    end
  end)
end

-- Record focus from aerospace on-focus-changed callback
-- Called via: hs -c "aeroSwitcher.recordFocus('$AEROSPACE_WINDOW_ID')"
function M.recordFocus(windowId)
  if windowId and windowId ~= "" then
    recordAccess("window:" .. windowId, "window")
  end
end

function M.show()
  if chooser then
    chooser:delete()
  end

  chooser = hs.chooser.new(onSelect)
  chooserStyle.apply(chooser)
  chooser:choices(cachedItems)
  chooser:show()

  refreshItems(function()
    if chooser then
      chooser:choices(cachedItems)
    end
  end)
end

-- Preload on module load
refreshItems()

return M
