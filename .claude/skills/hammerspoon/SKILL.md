---
name: hammerspoon
description: Configure and manage Hammerspoon automation, window management, key remapping, and app launching. Use when working with ~/.config/hammerspoon config files, adding keybindings, implementing window layouts, or troubleshooting Hammerspoon functionality.
---

# Hammerspoon configuration skill

## Overview

Manage Hammerspoon configuration for macOS automation including window management, app launching, and window switching.

## Configuration locations

- Config directory: `~/.config/hammerspoon/`
- Symlink: `~/.hammerspoon` → `~/.config/hammerspoon/`
- CLI tool: `hs` (installed via `hs.ipc.cliInstall()` in init.lua)

## Current modules

### Core infrastructure

**hyper-key.lua** - Inline hyper key implementation
- No spoon dependencies
- Provides `HyperKey.new(mods)` constructor
- Binding methods: `bind(key):toFunction(name, fn)` and `bind(key):toApplication(app)`
- Tracks bindings in `self.bindings` table

**config-watch.lua** - Auto-reload watcher
- Monitors `~/.config/hammerspoon/` for `.lua` file changes
- Automatically reloads Hammerspoon when files change
- Uses `hs.pathwatcher` API

**init.lua** - Main entry point
- Calls `hs.ipc.cliInstall()` to enable CLI access
- Defines `hyper` modifier: `{cmd, ctrl, alt, shift}`
- Defines `super` modifier: `{cmd, ctrl, alt}`
- Loads all modules: hyper-key, config-watch, window-hotkeys, quick-switch, window-switcher
- Test bindings: `hyper+r` (reload), `hyper+t` (test alert)

### Window management

**window-management.lua** - Grid-based window functions
- Dynamic grid system adjusts to screen type:
  - Normal screens: 8x4 grid
  - Ultrawide screens (aspect > 2.5): 10x4 grid
  - Vertical screens (aspect < 1): 4x8 grid
- Screen watcher automatically adjusts grid on display changes
- Core functions: maximize, center, halves, corners, thirds, two-thirds, throw (multi-display), resize (40px), nudge (40px)

**window-hotkeys.lua** - Comprehensive keybindings
- `super` key (cmd+ctrl+alt) for window operations:
  - `super+f` - Maximize, `super+c` - Center
  - `super+h/j/k/l` - Left/bottom/top/right halves
  - `super+u/i/n/m` - Corners
  - `super+d/e/g` - Thirds
  - `super+s/t` - Two-thirds
  - `super+q/w/a/z` - Throw to displays
- `hyper` key (super+shift) for resize: `hyper+h/j/k/l`
- `super+option` for nudge: `super+option+h/j/k/l`

### App launcher

**quick-switch.lua** - Direct app launch keybindings
- Uses `toApplication()` method from HyperKey
- Current app bindings:
  - `hyper+return` - Ghostty
  - `hyper+c` - Google Chrome
  - `hyper+s` - Spotify
  - `hyper+1` - 1Password
- Easy to add more apps: `hyper:bind("key"):toApplication("App Name")`

### Window switcher

**window-switcher.lua** - Window switcher using hs.window.switcher
- Uses built-in `hs.window.switcher` API
- Shows all windows across all spaces and screens
- UI features: thumbnails, titles, dark background theme
- Keybindings:
  - `hyper+tab` - Next window
  - `hyper+backtick` - Previous window

## Common operations

### Using the hs CLI

The `hs` command-line tool provides direct interaction with Hammerspoon. It requires `hs.ipc.cliInstall()` to be called in init.lua (already configured).

**Check for errors and module status:**
```bash
hs -c "
local status = {modules_loaded = {}, errors = {}}
local modules = {'hyper-key', 'config-watch', 'window-hotkeys', 'quick-switch', 'window-switcher'}
for _, mod in ipairs(modules) do
  local ok, result = pcall(require, mod)
  if ok then
    table.insert(status.modules_loaded, mod)
  else
    table.insert(status.errors, mod .. ': ' .. tostring(result))
  end
end
return hs.json.encode(status, true)
"
```

**View live console output:**
```bash
hs -C
```

**Execute Hammerspoon commands:**
```bash
hs -c "hs.alert.show('test')"
hs -c "hs.reload()"
```

**Interactive Lua REPL:**
```bash
hs
```

**Mirror prints to console:**
```bash
hs -P
```

**Run a script:**
```bash
hs /path/to/script.lua
```

**Common flags:**
- `-A` - Auto-launch Hammerspoon if not running
- `-C` - Clone console prints to this terminal (best for checking errors)
- `-P` - Mirror prints to Hammerspoon console
- `-c` - Execute command and return result
- `-i` - Force interactive mode
- `-n` - Disable colorized output
- `-N` - Force colorized output
- `-q` - Quiet mode (errors and results only)

### Other operations

**Reload Hammerspoon:**
```bash
hs -c "hs.reload()"
# or via URL:
open -g hammerspoon://reload
```

**Check if running:**
```bash
ps aux | rg -i hammerspoon
```

**Open console window:**
```bash
open "hammerspoon://consoleWindow"
```

**Test configuration:**
Edit any `.lua` file in `~/.config/hammerspoon/` to trigger auto-reload

## Development patterns

### Adding new keybindings

**Using hyper key:**
```lua
hyper:bind("key"):toFunction("Description", function()
  -- implementation
end)
```

**Using super key:**
```lua
super:bind("key"):toFunction("Description", function()
  -- implementation
end)
```

**Direct app launch:**
```lua
hyper:bind("t"):toApplication("Ghostty")
```

### Creating new modules

1. Create `~/.config/hammerspoon/module-name.lua`
2. Return module table or functionality
3. Require in `init.lua`: `local module = require("module-name")`
4. Auto-reload will trigger on save

### Window management patterns

**Getting focused window:**
```lua
local win = hs.window.focusedWindow()
if not win then return end
```

**Setting window frame:**
```lua
local screen = win:screen()
local max = screen:frame()
win:setFrame({
  x = max.x,
  y = max.y,
  w = max.w / 2,
  h = max.h
})
```

**Moving between displays:**
```lua
local nextScreen = screen:toEast()  -- or toWest(), toNorth(), toSouth()
if nextScreen then
  win:moveToScreen(nextScreen)
end
```

**Dynamic grid adjustment:**
```lua
local frame = screen:frame()
local aspectRatio = frame.w / frame.h
if aspectRatio > 2.5 then
  hs.grid.setGrid('10x4')  -- ultrawide
elseif aspectRatio < 1 then
  hs.grid.setGrid('4x8')   -- vertical
else
  hs.grid.setGrid('8x4')   -- normal
end
```

## App replacement status

Current Hammerspoon setup replaces:
- **Rectangle**: Window management via window-management.lua and window-hotkeys.lua
- **AltTab**: Window switching via window-switcher.lua
- **Raycast** (partial): App launching via quick-switch.lua

Still using:
- **Karabiner-Elements**: Key remapping (hyperspace key, vim arrows, etc.)
- **Raycast** (partial): Clipboard history, snippets, extensions

## Troubleshooting

### Check for errors
Use the `hs` CLI to check for module loading errors:
```bash
hs -c "
local status = {modules_loaded = {}, errors = {}}
local modules = {'hyper-key', 'config-watch', 'window-hotkeys', 'quick-switch', 'window-switcher'}
for _, mod in ipairs(modules) do
  local ok, result = pcall(require, mod)
  if ok then
    table.insert(status.modules_loaded, mod)
  else
    table.insert(status.errors, mod .. ': ' .. tostring(result))
  end
end
return hs.json.encode(status, true)
"
```

Or view live console output: `hs -C`

### Config not loading
- Check symlink: `ls -la ~/.hammerspoon`
- Verify Hammerspoon is running: `ps aux | rg Hammerspoon`
- Reload manually: `hs -c "hs.reload()"`
- Check for errors: `hs -C` or `open "hammerspoon://consoleWindow"`

### Keybindings not working
- Check for conflicts with app shortcuts
- Verify modifier keys are correct
- Test with: `hs -c "hs.alert.show('test')"`

### Auto-reload not triggering
- Verify `config-watch.lua` is loaded in `init.lua`
- Check file extension is `.lua`
- Restart Hammerspoon app

### CLI not working
If `hs` command not found, ensure `hs.ipc.cliInstall()` is called in init.lua

## Philosophy

### Zero spoons approach
- Inline functionality instead of external dependencies
- Only use spoons if absolutely necessary
- Keep implementation simple and maintainable

**Decision criteria for using spoons:**
- Must provide significant value that's hard to inline
- Must be actively maintained
- Must be worth the workflow complexity

**Future spoon management (not yet implemented):**
If spoons are needed, they will be managed via GitHub workflow:
1. Add spoon commit hash to `.github/workflows/versions.lua`
2. Create `.github/workflows/hammerspoon.yml` build workflow
3. Workflow clones spoons at specified revisions, bundles into tarball
4. Release as `hammerspoon-YYYY.MM.DD-darwin-arm64.tar.gz`
5. Only add spoons to config after workflow builds successfully

This ensures pinned, reproducible spoon dependencies similar to other tools in the repo.

### File organization
- One module per file
- Clear, descriptive names
- Require modules in `init.lua`
- Use XDG-style config directory

## Resources

- Hammerspoon API: https://www.hammerspoon.org/docs/
- Plan document: `~/.claude/plans/hammerspoon-consolidation.md`
- dbalatero dotfiles: https://github.com/dbalatero/dotfiles/tree/main/hammerspoon (reference only)
