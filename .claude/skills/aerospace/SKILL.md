---
name: aerospace
description: Configure and manage AeroSpace tiling window manager for macOS. Use when working with window layouts, tiling configurations, workspace management, monitor assignments, or AeroSpace keybindings. Keywords: aerospace, tiling, workspaces, window manager, i3-like.
---

# AeroSpace configuration skill

## Overview

AeroSpace is an i3-like tiling window manager for macOS. It uses a tree-based tiling paradigm where windows are organized in nested containers with different layouts and orientations.

## Configuration location

- Config file: `~/.config/aerospace/aerospace.toml`
- Alternative: `~/.aerospace.toml`
- Format: TOML
- Default config: `/Applications/AeroSpace.app/Contents/Resources/default-config.toml`

## Current configuration

### Workspace organization

**Internal monitor (built-in) - workspaces 1-5:**
- Windows automatically maximize with fullscreen mode
- No gaps for maximum screen usage
- Perfect for focused single-window work

**External monitor - workspaces 6-9:**
- Three-column tiling layout
- 8px gaps between windows
- Flexible window arrangement

### Workspace assignment

```toml
[workspace-to-monitor-force-assignment]
# Internal monitor
1 = 'built-in'
2 = 'built-in'
3 = 'built-in'
4 = 'built-in'
5 = 'built-in'

# External monitor (sequence number 2)
6 = 2
7 = 2
8 = 2
9 = 2
```

### Gap configuration

```toml
[gaps]
inner.horizontal = 8
inner.vertical = 8
outer.left = 8
outer.bottom = 8
outer.top = 8
outer.right = 8
```

## Key bindings

### Window navigation
- `alt-h/j/k/l` - Focus left/down/up/right window (vim-style)

### Window movement
- `alt-shift-h/j/k/l` - Move window left/down/up/right

### Workspace switching
- `alt-1` through `alt-9` - Switch to workspace 1-9

### Move window to workspace
- `alt-shift-1` through `alt-9` - Move window to workspace and follow

### Monitor management
- `alt-,` - Focus left monitor
- `alt-.` - Focus right monitor
- `alt-shift-,` - Move workspace to left monitor
- `alt-shift-.` - Move workspace to right monitor

### Layout controls
- `alt-shift-space` - Toggle floating/tiling
- `alt-f` - Toggle fullscreen
- `alt-shift-v` - Create vertical split (for three-column layout)
- `alt-shift-o` - Toggle horizontal/vertical orientation

### Resize mode
- `alt-r` - Enter resize mode
  - `h` - Decrease width
  - `j` - Increase height
  - `k` - Decrease height
  - `l` - Increase width
  - `enter` or `esc` - Exit resize mode

### Other
- `alt-shift-r` - Reload config
- `alt-shift-q` - Close window

## Core concepts

### Tree-based tiling

Every workspace contains a tree where:
- **Containers** have a layout and orientation
- **Windows** are leaf nodes
- Containers can be nested arbitrarily

### Layout types

1. **h_tiles** - Horizontal tiles (side-by-side)
2. **v_tiles** - Vertical tiles (stacked)
3. **h_accordion** - Horizontal accordion (overlapping)
4. **v_accordion** - Vertical accordion (overlapping)

### Normalization

Two automatic normalizations keep layouts sensible:
1. Single-child containers are flattened
2. Nested containers must have opposite orientations

### Creating three-column layout

1. Open first window (takes full width)
2. Open second window (splits horizontally - two columns)
3. Focus second window and press `alt-shift-v` to create vertical split
4. Open third window (appears in the vertical split)

Result: `[Window 1] [Window 2 over Window 3]`

## Common operations

### List monitors
```bash
aerospace list-monitors
aerospace list-monitors --format '%{monitor-id} | %{monitor-name}'
```

### List workspaces
```bash
aerospace list-workspaces --monitor all
aerospace list-workspaces --monitor focused
```

### List windows
```bash
aerospace list-windows --workspace focused
aerospace list-windows --all
```

### Manual layout commands
```bash
# Change layout
aerospace layout h_tiles      # Horizontal tiles
aerospace layout v_tiles      # Vertical tiles
aerospace layout horizontal   # Change orientation to horizontal
aerospace layout vertical     # Change orientation to vertical

# Toggle layouts
aerospace layout tiles accordion              # Toggle tile vs accordion
aerospace layout horizontal vertical          # Toggle orientation
aerospace layout floating tiling              # Toggle float vs tiling

# Join windows
aerospace join-with right     # Create nested vertical split
aerospace join-with left      # Create nested vertical split on left
aerospace join-with up        # Create nested horizontal split above
aerospace join-with down      # Create nested horizontal split below

# Move windows
aerospace move left
aerospace move right
aerospace move up
aerospace move down

# Resize windows
aerospace resize smart +50    # Smart resize (larger)
aerospace resize smart -50    # Smart resize (smaller)
aerospace resize width +100
aerospace resize height -50

# Flatten tree
aerospace flatten-workspace-tree
```

### Reload config
```bash
aerospace reload-config
# or use keybinding: alt-shift-r
```

## Monitor patterns

Workspace assignment supports:
- `main` - Primary monitor
- `secondary` - Non-main monitor (2-monitor setups only)
- `<number>` - Monitor sequence (1-based, left to right)
- `<regex>` - Case-insensitive substring match
- `'^pattern$'` - Full regex match

Example:
```toml
[workspace-to-monitor-force-assignment]
1 = 'main'
2 = 'secondary'
3 = 1                    # First monitor (leftmost)
4 = 2                    # Second monitor
5 = 'built-in'           # Substring match
6 = '^dell.*'            # Regex match
7 = ['secondary', 'dell'] # Multiple patterns (fallback)
```

## Configuration examples

### Per-monitor gaps
```toml
[gaps]
# Different gaps per monitor
inner.horizontal = [{ monitor.main = 0 }, { monitor.secondary = 8 }, 5]
outer.left = [{ monitor.'built-in' = 0 }, 10]
```

### Window callbacks
```toml
# Auto-tile all new windows
[[on-window-detected]]
if.during-aerospace-startup = false
run = 'layout tiling'

# Specific workspace behavior
[[on-window-detected]]
if.workspace = [1, 2, 3]
run = 'fullscreen on --no-outer-gaps'

# App-specific layout
[[on-window-detected]]
if.app-name-regex-substring = 'Terminal'
run = 'move-node-to-workspace 1'
```

### Custom modes
```toml
[mode.service.binding]
esc = 'mode main'
r = ['flatten-workspace-tree', 'mode main']
f = ['layout floating tiling', 'mode main']
h = ['layout horizontal vertical', 'mode main']

[mode.main.binding]
alt-shift-semicolon = 'mode service'
```

## Troubleshooting

### Check if AeroSpace is running
```bash
ps aux | rg -i aerospace
```

### View current workspace
```bash
aerospace list-workspaces --monitor focused --empty no
```

### View window tree
```bash
aerospace list-windows --workspace focused --format '%{window-id} | %{app-name} | %{window-title}'
```

### Config not loading
- Check config file location: `~/.config/aerospace/aerospace.toml`
- Validate TOML syntax
- Reload config: `aerospace reload-config` or `alt-shift-r`
- Check AeroSpace logs in Console.app

### Keybindings not working
- Check for conflicts with other apps (Hammerspoon, Karabiner, etc.)
- Verify modifier keys are correct
- Test with: `aerospace list-workspaces --monitor focused`

### Windows not tiling
- Check if window is floating: `aerospace list-windows --workspace focused`
- Toggle tiling: `alt-shift-space`
- Some apps force floating mode

## Resources

- Repository: https://github.com/nikitabobko/AeroSpace
- Documentation: https://github.com/nikitabobko/AeroSpace/blob/main/docs/guide.adoc
- Default config: https://github.com/nikitabobko/AeroSpace/blob/main/docs/config-examples/default-config.toml
- i3-like config example: https://github.com/nikitabobko/AeroSpace/blob/main/docs/config-examples/i3-like-config-example.toml
