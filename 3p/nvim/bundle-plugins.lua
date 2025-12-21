-- Bundle nvim plugins using vim.pack.add
-- Usage: nvim --headless -c "luafile 3p/nvim/bundle-plugins.lua" -c "qa"
-- Expects: NVIM_PACK_LOCK environment variable pointing to nvim-pack-lock.json

local function read_file(path)
  local f = io.open(path, "r")
  if not f then
    return nil, "failed to open file: " .. path
  end
  local content = f:read("*all")
  f:close()
  return content
end

local function parse_json(str)
  local ok, result = pcall(vim.json.decode, str)
  if not ok then
    return nil, "failed to parse json: " .. tostring(result)
  end
  return result
end

local function bundle_plugins(lock_file)
  print("reading pack lock from: " .. lock_file)

  local content, err = read_file(lock_file)
  if not content then
    return nil, err
  end

  local lock, err = parse_json(content)
  if not lock then
    return nil, err
  end

  local plugins = {}
  for name, info in pairs(lock.plugins) do
    print("adding plugin: " .. name .. " @ " .. info.rev)
    table.insert(plugins, {
      src = info.src,
      version = info.rev
    })
  end

  print("bundling " .. #plugins .. " plugins via vim.pack.add")
  vim.pack.add(plugins, { confirm = false, load = false })

  print("plugin bundling complete")
  return true
end

local function main()
  local lock_file = os.getenv("NVIM_PACK_LOCK")
  if not lock_file or lock_file == "" then
    error("NVIM_PACK_LOCK environment variable not set")
  end

  local ok, err = bundle_plugins(lock_file)
  if not ok then
    io.stderr:write("error: " .. tostring(err) .. "\n")
    os.exit(1)
  end
end

main()
