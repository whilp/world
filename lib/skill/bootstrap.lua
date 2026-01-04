-- bootstrap validation for cosmic development environments
-- checks if SessionStart hook properly configures lua/cosmic-lua
-- usage: cosmic-lua -l skill bootstrap

local cosmo = require("cosmo")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local function log(msg)
  io.stderr:write("bootstrap: " .. msg .. "\n")
end

local function check_file(filepath)
  return path.exists(filepath)
end

local function check_lua_available()
  -- check if lua is available in PATH
  local handle = spawn({"which", "lua"})
  local ok, out = handle:read()
  if ok and out and out ~= "" then
    return true, out:match("^%s*(.-)%s*$")
  end
  return false
end

local function check_settings_file()
  -- check for .claude/settings or .claude/settings.json
  local settings_path = ".claude/settings"
  local settings_json_path = ".claude/settings.json"

  if check_file(settings_path) then
    return settings_path
  elseif check_file(settings_json_path) then
    return settings_json_path
  end

  return nil
end

local function parse_settings(filepath)
  local content = cosmo.Slurp(filepath)
  if not content then
    return nil, "failed to read " .. filepath
  end

  -- try parsing as JSON
  local ok, data = pcall(cosmo.DecodeJson, content)
  if ok and type(data) == "table" then
    return data
  end

  return nil, "failed to parse " .. filepath .. " as JSON"
end

local function check_session_start_hook(settings)
  if not settings.hooks then
    return false
  end

  if not settings.hooks.SessionStart then
    return false
  end

  -- check if SessionStart has at least one hook configured
  if type(settings.hooks.SessionStart) == "table" and #settings.hooks.SessionStart > 0 then
    return true, settings.hooks.SessionStart
  end

  return false
end

local function check_bootstrap_setup()
  local issues = {}
  local has_hook = false
  local has_lua = false

  -- check for settings file
  local settings_file = check_settings_file()
  if not settings_file then
    table.insert(issues, "no .claude/settings or .claude/settings.json file found")
  else
    -- parse settings
    local settings, err = parse_settings(settings_file)
    if not settings then
      table.insert(issues, err)
    else
      -- check for SessionStart hook
      local hook_exists, hooks = check_session_start_hook(settings)
      if not hook_exists then
        table.insert(issues, "no SessionStart hook configured in " .. settings_file)
      else
        has_hook = true
      end
    end
  end

  -- check if lua is available
  local lua_ok, lua_path = check_lua_available()
  if lua_ok then
    has_lua = true
  else
    table.insert(issues, "lua not found in PATH")
  end

  -- check for bin/lua or bin/cosmic-lua
  local bin_lua = check_file("bin/lua")
  local bin_cosmic_lua = check_file("bin/cosmic-lua")

  return {
    has_hook = has_hook,
    has_lua = has_lua,
    has_bin_lua = bin_lua,
    has_bin_cosmic_lua = bin_cosmic_lua,
    settings_file = settings_file,
    lua_path = lua_path,
    issues = issues,
  }
end

local function print_instructions(result)
  print([[
## Bootstrap setup not complete

To set up a bootstrap environment similar to the reference configuration:

### For make-based projects:

1. Create a bootstrap target in your Makefile:
   ```makefile
   .PHONY: bootstrap
   bootstrap: bin/lua
   ifdef CLAUDE_ENV_FILE
   	@echo 'export PATH="$(PWD)/bin:$$PATH"' >> "$(CLAUDE_ENV_FILE)"
   endif

   bin/lua: bin/cosmic-lua
   	@mkdir -p bin
   	ln -sf cosmic-lua $@

   bin/cosmic-lua:
   	@mkdir -p bin
   	curl -L -o $@ https://github.com/whilp/world/releases/latest/download/lua
   	chmod +x $@
   ```

2. Add bin/ to .gitignore:
   ```bash
   echo 'bin/' >> .gitignore
   ```

3. Create or update .claude/settings.json:
   ```json
   {
     "hooks": {
       "SessionStart": [
         {
           "hooks": [
             {
               "type": "command",
               "command": "make bootstrap"
             }
           ]
         }
       ]
     }
   }
   ```

### For non-make projects:

1. Create a bootstrap script (.claude/hooks/bootstrap.sh):
   ```bash
   #!/bin/bash
   set -euo pipefail

   # download cosmic-lua if not present
   if [ ! -f bin/cosmic-lua ]; then
     mkdir -p bin
     curl -L -o bin/cosmic-lua https://github.com/whilp/world/releases/latest/download/lua
     chmod +x bin/cosmic-lua
   fi

   # create symlink
   if [ ! -f bin/lua ]; then
     ln -sf cosmic-lua bin/lua
   fi

   # export PATH if CLAUDE_ENV_FILE is set
   if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
     echo "export PATH=\"$PWD/bin:\$PATH\"" >> "$CLAUDE_ENV_FILE"
   fi
   ```

2. Make it executable:
   ```bash
   chmod +x .claude/hooks/bootstrap.sh
   ```

3. Create or update .claude/settings.json:
   ```json
   {
     "hooks": {
       "SessionStart": [
         {
           "hooks": [
             {
               "type": "command",
               "command": "$CLAUDE_PROJECT_DIR/.claude/hooks/bootstrap.sh"
             }
           ]
         }
       ]
     }
   }
   ```

4. Add bin/ to .gitignore:
   ```bash
   echo 'bin/' >> .gitignore
   ```

### Current status:
]])

  if result.settings_file then
    print("- ✓ Settings file found: " .. result.settings_file)
  else
    print("- ✗ No settings file")
  end

  if result.has_hook then
    print("- ✓ SessionStart hook configured")
  else
    print("- ✗ No SessionStart hook")
  end

  if result.has_lua then
    print("- ✓ lua available in PATH: " .. (result.lua_path or ""))
  else
    print("- ✗ lua not in PATH")
  end

  if result.has_bin_lua then
    print("- ✓ bin/lua exists")
  else
    print("- ✗ bin/lua not found")
  end

  if result.has_bin_cosmic_lua then
    print("- ✓ bin/cosmic-lua exists")
  else
    print("- ✗ bin/cosmic-lua not found")
  end

  if #result.issues > 0 then
    print("\nIssues detected:")
    for _, issue in ipairs(result.issues) do
      print("  - " .. issue)
    end
  end
end

local function main()
  local result = check_bootstrap_setup()

  -- consider it valid if we have either:
  -- 1. hook + lua available, or
  -- 2. bin/lua or bin/cosmic-lua exists (partial setup)
  local is_valid = (result.has_hook and result.has_lua) or
                   result.has_bin_lua or
                   result.has_bin_cosmic_lua

  if is_valid and #result.issues == 0 then
    print("✓ Bootstrap setup looks good!")
    if result.has_lua then
      print("  lua: " .. (result.lua_path or "unknown"))
    end
    if result.has_hook then
      print("  SessionStart hook: configured")
    end
    return 0
  else
    print_instructions(result)
    return 1
  end
end

if cosmo.is_main() then
  local code = main()
  os.exit(code)
end

return {
  check_bootstrap_setup = check_bootstrap_setup,
  check_lua_available = check_lua_available,
  check_settings_file = check_settings_file,
  parse_settings = parse_settings,
  check_session_start_hook = check_session_start_hook,
  main = main,
}
