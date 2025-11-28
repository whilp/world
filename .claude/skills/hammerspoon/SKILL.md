---
name: hammerspoon
description: Configure and manage Hammerspoon automation, window management, key remapping, and app launching. Use when working with ~/.config/hammerspoon config files, adding keybindings, implementing window layouts, or troubleshooting Hammerspoon functionality.
---

# Hammerspoon configuration skill

## Overview

Manage Hammerspoon configuration for macOS automation including window management, key remapping, app launching, and system automation.

## Configuration locations

- Config directory: `~/.config/hammerspoon/`
- Symlink: `~/.hammerspoon` → `~/.config/hammerspoon/`
- Console logs: `~/Library/Logs/Hammerspoon/console.log`

## Current implementation

### Core modules (Phase 1.1 ✅)

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
- Defines `hyper` modifier: `{cmd, ctrl, alt, shift}`
- Loads all modules: hyper-key, config-watch, window-hotkeys
- Current bindings:
  - `hyper+r` - Reload config manually
  - `hyper+t` - Test alert
- Shows "Hammerspoon loaded" alert on startup

### Window management (Phase 1.2 ✅)

**window-management.lua** - Grid-based window functions
- Dynamic grid system adjusts to screen type:
  - Normal screens: 8x4 grid
  - Ultrawide screens (aspect > 2.5): 10x4 grid
  - Vertical screens (aspect < 1): 4x8 grid
- Screen watcher automatically adjusts grid on display changes
- Core functions:
  - `maximizeWindow()`, `centerOnScreen()`
  - `leftHalf()`, `rightHalf()`, `topHalf()`, `bottomHalf()`
  - `topLeft()`, `topRight()`, `bottomLeft()`, `bottomRight()` - Corners
  - `leftThird()`, `centerThird()`, `rightThird()` - Thirds
  - `leftTwoThirds()`, `rightTwoThirds()` - Two-thirds
  - `throwLeft()`, `throwRight()`, `throwUp()`, `throwDown()` - Multi-display
  - `shrinkLeft()`, `growRight()`, `shrinkUp()`, `growDown()` - Resize (40px)
  - `nudgeLeft()`, `nudgeRight()`, `nudgeUp()`, `nudgeDown()` - Nudge (40px)

**window-hotkeys.lua** - Comprehensive keybindings
- Uses `super` key (cmd+ctrl+alt) for all window operations:
  - `super+f` - Maximize, `super+c` - Center
  - `super+h/j/k/l` - Left/bottom/top/right halves
  - `super+u/i/n/m` - Corners (top-left/top-right/bottom-left/bottom-right)
  - `super+d/e/g` - Thirds (left/center/right)
  - `super+s/t` - Two-thirds (left/right)
  - `super+q/w/a/z` - Throw to displays (left/right/up/down)
- Uses `hyper` key (super+shift) for resize:
  - `hyper+h/j/k/l` - Shrink left, grow down, shrink up, grow right
- Uses `super+option` for nudge:
  - `super+option+h/j/k/l` - Nudge left/down/up/right

## Planned modules

### Phase 2: App launcher and automation
- `quick-switch.lua` - Direct app launch keybindings
- `text-expander.lua` - Trie-based snippet expansion
- `audio-switcher.lua` - Audio device chooser
- `mute-on-sleep.lua` - Auto-mute on wake

### Phase 3: Key remapping
- Vim arrow keys (option+hjkl)
- Emacs navigation (ctrl+n/p)
- Page navigation (cmd+shift+j/k)

### Phase 4: Advanced features
- Window switcher (AltTab replacement)
- Monitor input switching
- Custom automation

## Common operations

### Reload Hammerspoon
```bash
open -g hammerspoon://reload
```

### Check if running
```bash
ps aux | rg -i hammerspoon
```

### View console logs
```bash
tail -f ~/Library/Logs/Hammerspoon/console.log
```

### Test configuration
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

## Migration tracking

Plan location: `~/.claude/plans/hammerspoon-consolidation.md`

### Apps to replace
- Rectangle (window management) - Phase 1
- Raycast (app launcher, snippets) - Phase 2
- Karabiner-Elements (key remapping) - Phase 3
- AltTab (window switching) - Phase 4

### Migration strategy
1. Implement Hammerspoon equivalent
2. Run in parallel with existing app
3. Test for 1-2 weeks
4. Disable/uninstall old app when confident

## Troubleshooting

### Config not loading
- Check symlink: `ls -la ~/.hammerspoon`
- Verify Hammerspoon is running: `ps aux | rg Hammerspoon`
- Reload manually: `open -g hammerspoon://reload`

### Keybindings not working
- Check for conflicts with app shortcuts
- Verify modifier keys are correct
- Test with simple alert: `hs.alert.show("test")`

### Auto-reload not triggering
- Verify `config-watch.lua` is loaded in `init.lua`
- Check file extension is `.lua`
- Restart Hammerspoon app

## Philosophy

### Zero spoons approach
- Inline functionality instead of external dependencies
- Only use spoons if absolutely necessary
- Keep implementation simple and maintainable
- If spoons needed: create GitHub workflow for bundling

### File organization
- One module per file
- Clear, descriptive names
- Require modules in `init.lua`
- Use XDG-style config directory

## Resources

- Hammerspoon API: https://www.hammerspoon.org/docs/
- Plan document: `~/.claude/plans/hammerspoon-consolidation.md`
- dbalatero dotfiles: https://github.com/dbalatero/dotfiles/tree/main/hammerspoon (reference only)
