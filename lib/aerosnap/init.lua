local unix = require("cosmo.unix")
local path = require("cosmo.path")
local sqlite3 = require("cosmo.lsqlite3")
local spawn = require("spawn").spawn

local DB_DIR = path.join(os.getenv("HOME"), ".cache", "aerospace")
local DB_FILE = path.join(DB_DIR, "window-mappings.db")

local function open_db()
  unix.makedirs(DB_DIR)
  local db = sqlite3.open(DB_FILE)
  db:exec([[
    create table if not exists window_mappings (
      id integer primary key autoincrement,
      app_bundle_id text not null,
      app_name text,
      window_title text not null,
      workspace text not null,
      created_at text default current_timestamp,
      updated_at text default current_timestamp
    );
    create unique index if not exists idx_bundle_title
      on window_mappings(app_bundle_id, window_title);
  ]])
  return db
end

local function get_focused_window()
  local handle = spawn({
    "aerospace", "list-windows", "--focused",
    "--format", "%{window-id}\t%{app-bundle-id}\t%{app-name}\t%{workspace}\t%{window-title}"
  })
  if not handle then return nil, "failed to get focused window" end
  local _, output = handle:read()
  if not output or output == "" then
    return nil, "no focused window"
  end

  local pat = "^(%d+)\t([^\t]*)\t([^\t]*)\t([^\t]*)\t([^\n]*)"
  local window_id, bundle_id, app_name, workspace, title = output:match(pat)
  if window_id then
    return {
      ["window-id"] = tonumber(window_id),
      ["app-name"] = app_name,
      ["app-bundle-id"] = bundle_id,
      ["workspace"] = workspace,
      ["window-title"] = title or "",
    }
  end
  return nil, "failed to parse focused window"
end

local function get_all_windows()
  local handle = spawn({
    "aerospace", "list-windows", "--all",
    "--format", "%{window-id}\t%{app-bundle-id}\t%{app-name}\t%{workspace}\t%{window-title}"
  })
  if not handle then return nil, "failed to get windows" end
  local _, output = handle:read()
  if not output or output == "" then
    return nil, "failed to get windows"
  end

  local windows = {}
  for line in output:gmatch("[^\n]+") do
    local window_id, bundle_id, app_name, workspace, title = line:match("^(%d+)\t([^\t]*)\t([^\t]*)\t([^\t]*)\t(.*)")
    if window_id then
      table.insert(windows, {
        ["window-id"] = tonumber(window_id),
        ["app-name"] = app_name,
        ["app-bundle-id"] = bundle_id,
        ["workspace"] = workspace,
        ["window-title"] = title or "",
      })
    end
  end
  return windows
end

local function lookup_workspace(db, bundle_id, window_title)
  local target_workspace = nil
  local match_type = nil

  local stmt = db:prepare([[
    select workspace, window_title from window_mappings
    where app_bundle_id = ?
    order by
      case when window_title = ? then 0 else 1 end,
      updated_at desc
    limit 1
  ]])
  stmt:bind_values(bundle_id, window_title or "")
  for row in stmt:nrows() do
    target_workspace = row.workspace
    if row.window_title == (window_title or "") then
      match_type = "exact"
    else
      match_type = "bundle"
    end
  end
  stmt:finalize()

  return target_workspace, match_type
end

local function cmd_record(_args)
  local win, err = get_focused_window()
  if not win then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end

  if not win["app-bundle-id"] or win["app-bundle-id"] == "" then
    io.stderr:write("error: window has no bundle id\n")
    return 1
  end

  local db = open_db()
  local stmt = db:prepare([[
    insert into window_mappings (app_bundle_id, app_name, window_title, workspace)
    values (?, ?, ?, ?)
    on conflict(app_bundle_id, window_title) do update set
      app_name = excluded.app_name,
      workspace = excluded.workspace,
      updated_at = current_timestamp
  ]])
  stmt:bind_values(
    win["app-bundle-id"],
    win["app-name"] or "",
    win["window-title"] or "",
    win.workspace
  )
  stmt:step()
  stmt:finalize()
  db:close()

  io.write(string.format("recorded: %s '%s' -> workspace %s\n",
    win["app-name"] or win["app-bundle-id"],
    win["window-title"] or "(untitled)",
    win.workspace))
  return 0
end

local function cmd_load(_args)
  local db = open_db()
  local current_windows, err = get_all_windows()
  if not current_windows then
    io.stderr:write("error: " .. err .. "\n")
    db:close()
    return 1
  end

  local moved, matched = 0, 0

  for _, win in ipairs(current_windows) do
    local target_workspace, match_type = lookup_workspace(db, win["app-bundle-id"], win["window-title"])

    if target_workspace and target_workspace ~= win.workspace then
      spawn({"aerospace", "move-node-to-workspace", target_workspace,
             "--window-id", tostring(win["window-id"])}):wait()
      moved = moved + 1
      io.write(string.format("moved (%s): %s '%s' -> %s\n",
        match_type,
        win["app-name"] or win["app-bundle-id"],
        win["window-title"] or "(untitled)",
        target_workspace))
    elseif target_workspace then
      matched = matched + 1
    end
  end

  db:close()
  io.write(string.format("already in place: %d, moved: %d\n", matched, moved))
  return 0
end

local function cmd_show(_args)
  local db = open_db()
  io.write("window mappings:\n")
  io.write(string.format("%-30s %-40s %s\n", "app", "title", "workspace"))
  io.write(string.rep("-", 80) .. "\n")
  for row in db:nrows([[
    select app_name, app_bundle_id, window_title, workspace, updated_at
    from window_mappings
    order by workspace, app_name, window_title
  ]]) do
    local app = row.app_name
    if #app > 28 then app = app:sub(1, 28) .. ".." end
    local title = row.window_title
    if #title > 38 then title = title:sub(1, 38) .. ".." end
    io.write(string.format("%-30s %-40s %s\n", app, title, row.workspace))
  end
  db:close()
  return 0
end

local function cmd_seed(_args)
  local windows, err = get_all_windows()
  if not windows then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end

  local db = open_db()
  local stmt = db:prepare([[
    insert into window_mappings (app_bundle_id, app_name, window_title, workspace)
    values (?, ?, ?, ?)
    on conflict(app_bundle_id, window_title) do update set
      app_name = excluded.app_name,
      workspace = excluded.workspace,
      updated_at = current_timestamp
  ]])

  local recorded = 0
  for _, win in ipairs(windows) do
    if win["app-bundle-id"] and win["app-bundle-id"] ~= "" then
      stmt:bind_values(
        win["app-bundle-id"],
        win["app-name"] or "",
        win["window-title"] or "",
        win.workspace
      )
      stmt:step()
      stmt:reset()
      recorded = recorded + 1
    end
  end
  stmt:finalize()
  db:close()

  io.write(string.format("recorded %d windows\n", recorded))
  return 0
end

local function cmd_clear(_args)
  local db = open_db()
  db:exec("delete from window_mappings")
  db:close()
  io.write("cleared all window mappings\n")
  return 0
end

local function cmd_help()
  io.write("aerosnap - record and restore window-to-workspace mappings\n")
  io.write("\n")
  io.write("usage: aerosnap <command>\n")
  io.write("\n")
  io.write("commands:\n")
  io.write("  record  save focused window's workspace assignment\n")
  io.write("  seed    save all current windows' workspace assignments\n")
  io.write("  load    restore windows to their recorded workspaces\n")
  io.write("  show    display all recorded mappings\n")
  io.write("  clear   remove all mappings\n")
  io.write("  help    show this help\n")
  io.write("\n")
  io.write("database: " .. DB_FILE .. "\n")
  return 0
end

local function cmd_unknown(command)
  io.stderr:write("unknown command: " .. command .. "\n")
  io.stderr:write("run 'aerosnap help' for usage\n")
  return 1
end

local commands = {
  record = cmd_record,
  seed = cmd_seed,
  load = cmd_load,
  show = cmd_show,
  clear = cmd_clear,
  help = cmd_help,
}

local function main(args)
  if #args == 0 then
    return cmd_help()
  end

  local command = args[1]
  local cmd_args = {table.unpack(args, 2)}
  local cmd_fn = commands[command]

  if cmd_fn then
    return cmd_fn(cmd_args)
  else
    return cmd_unknown(command)
  end
end

return {
  main = main,
  get_all_windows = get_all_windows,
  get_focused_window = get_focused_window,
  lookup_workspace = lookup_workspace,
}
