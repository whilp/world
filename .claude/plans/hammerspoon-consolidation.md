# Hammerspoon consolidation plan

## Overview

Consolidate Rectangle, Karabiner-Elements, AltTab, and Raycast functionality into Hammerspoon for a unified, programmable macOS automation system.

## Claude Code skill

**Location:** `~/.claude/skills/hammerspoon/SKILL.md`

This skill should be updated as new modules and features are implemented. Track additions here:

**Skill updates log:**
- 2025-11-28: Initial skill created with Phase 1.1 implementation (hyper-key, config-watch, init.lua)
- Next update: Add Phase 1.2 window management implementation details

## Current state analysis

### Apps to potentially replace
- **Rectangle**: Window management (98 shortcuts configured)
- **Karabiner-Elements**: 9 key remapping rules (vim arrows, hyperspace, ctrl-n/p)
- **AltTab**: Window switching
- **Raycast**: App launcher, clipboard history, snippets, extensions

### Current Karabiner rules
1. Option+hjkl → arrow keys
2. Control+n/p → down/up arrows
3. Cmd+Shift+j/k → Shift+PageDown/PageUp
4. Enter as hyperspace (tap=enter, hold=hyper modifier)

### Current Rectangle shortcuts
- Window halves (left/right/top/bottom)
- Corners (4 positions)
- Thirds and sixths
- Maximize, center
- Display switching
- Resize larger/smaller
- Todo sidebar mode

## Dependency management philosophy

### Spoons: only if necessary, only through workflow

**Decision criteria for using spoons:**
- Must provide significant value that's hard to inline
- Must be actively maintained
- Must be worth the workflow complexity

**Current assessment:**
- **HyperKey.spoon**: Can inline - it's ~90 lines plus overlays we might not need
- **VimMode.spoon**: Skip - not needed for this workflow
- **SkyRocket.spoon**: Skip - mouse-based window management is optional
- **EmmyLua.spoon**: Skip - LSP is nice-to-have, not essential

**Conclusion**: Start with zero spoons, inline what we need

### If we need spoons later: GitHub workflow pattern

**Only create this if we actually need spoons:**

**Files to create:**
- `.github/workflows/hammerspoon.yml` - Build workflow
- `.github/workflows/hammerspoon/build` - Build script (LuaJIT)
- Update `.github/workflows/versions.lua` - Add spoon commit hashes

**Build process:**
1. Read versions.lua for spoon commit hashes
2. Clone each spoon at specified revision
3. Remove .git directories
4. Bundle into single tarball: `hammerspoon-YYYY.MM.DD-darwin-arm64.tar.gz`
5. Calculate sha256 checksums
6. Create GitHub release with artifact

**versions.lua additions:**
```lua
return {
  luajit = "25a61a182166fec06f1a1a025eb8fabbb6cf483e",
  -- ... other existing dependencies

  -- Only add if actually needed:
  -- some_spoon = "abc123...",
}
```

**Important**: Spoons are NOT added to the config until the workflow exists and builds successfully

## Phase 1: Foundation and window management

### 1.1: Set up base Hammerspoon structure ✅

**Status:** Completed on 2025-11-28

**Files created:**
- `~/.config/hammerspoon/init.lua` - Main entry point
- `~/.config/hammerspoon/hyper-key.lua` - Inline hyper key implementation (from HyperKey.spoon)
- `~/.config/hammerspoon/config-watch.lua` - Auto-reload on changes

**Implementation notes:**
- Created symlink from `~/.hammerspoon` → `~/.config/hammerspoon/` (backed up existing directory to `~/.hammerspoon.backup`)
- Implemented hyper key module with `toFunction()` and `toApplication()` binding methods
- Config watcher monitors `~/.config/hammerspoon/` for `.lua` file changes and auto-reloads
- Added two test bindings:
  - `hyper+h` - Reload config manually
  - `hyper+t` - Test alert to verify hyper key works
- Both `hyper` (cmd+ctrl+alt+shift) and `super` (cmd+ctrl+alt) modifier objects created

**Testing checklist:**
- [x] Hyper key detection works (cmd+ctrl+alt+shift + key)
- [x] Auto-reload triggers when editing `.lua` files
- [x] Manual reload with hyper+h works
- [x] toFunction binding works (test alert)
- [ ] toApplication binding (will test in Phase 2.1)

**Next step:** Phase 1.2 - Implement window management

### 1.2: Implement window management

**Files to create:**
- `~/.config/hammerspoon/window-management.lua` - Grid-based window functions
- `~/.config/hammerspoon/window-hotkeys.lua` - Keybindings for window operations

**Core functions needed:**
- `maximizeWindow()` - Full screen
- `centerOnScreen()` - Center window
- `leftHalf()`, `rightHalf()` - Half screen layouts
- `topHalf()`, `bottomHalf()` - Vertical half layouts
- `throwLeft()`, `throwRight()` - Move between displays
- `shrinkLeft()`, `growRight()`, `shrinkUp()`, `growDown()` - Fine adjustments
- `nudgeLeft()`, `nudgeRight()`, `nudgeUp()`, `nudgeDown()` - Position adjustments

**Grid system:**
- 8x4 for normal screens
- 10x4 for ultrawide screens
- 4x8 for vertical screens
- Dynamic grid adjustment on screen changes

**Keybindings (super key prefix):**
- `f` - Maximize
- `c` - Center
- `h` - Left half
- `j` - Bottom half
- `k` - Top half
- `l` - Right half
- `q` - Throw to left display
- `w` - Throw to right display
- `n`,`,` - Shrink left/up
- `.`,`m` - Grow right/down
- `y`,`u`,`i`,`o` - Nudge left/down/up/right

**Testing:**
- Test each window operation
- Test with multiple displays
- Test grid system on different screen resolutions
- Compare behavior with Rectangle shortcuts

**Migration checklist:**
- [ ] Map all Rectangle shortcuts to Hammerspoon equivalents
- [ ] Test Rectangle todo mode (decide keep or replace)
- [ ] Disable Rectangle launch on login
- [ ] Run side-by-side for 1 week
- [ ] Uninstall Rectangle

### 1.3: Mouse-based window management (optional)

**Decision:** Skip initially - mouse-based window management is not essential

**If needed later:**
- Inline SkyRocket functionality (simple canvas-based drag handlers)
- Or use built-in `hs.grid` with mouse integration
- Or wait until we have spoon workflow, then bundle SkyRocket

**For now:** Focus on keyboard-based window management only

## Phase 2: App launcher and basic automation

### 2.1: Quick app switcher

**Files to create:**
- `~/.config/hammerspoon/quick-switch.lua` - Direct app launch keybindings

**Apps to bind (hyper key prefix):**
- `t` - Ghostty (terminal)
- `c` - Google Chrome
- `s` - Spotify
- `1` - 1Password
- Add more as needed

**Testing:**
- Test each app launch
- Test switching to already-running apps
- Test launching apps that aren't running

**Migration checklist:**
- [ ] Identify most-used Raycast app launches
- [ ] Add keybindings for top 10 apps
- [ ] Decide which Raycast features to keep (clipboard, extensions, etc.)

### 2.2: Text expansion

**Files to create:**
- `~/.config/hammerspoon/text-expander.lua` - Trie-based snippet expansion

**Initial snippets:**
- `+date` - Current date (formatted)
- `+email` - Email address
- `+time` - Current time
- Add personal snippets

**Features:**
- Trigger on space or return
- Support for dynamic snippets (functions)
- Trie-based matching for performance

**Testing:**
- Test basic string expansion
- Test dynamic date/time snippets
- Test in different applications
- Compare with Raycast snippet timing

**Migration checklist:**
- [ ] Export snippets from Raycast
- [ ] Convert to Hammerspoon format
- [ ] Test all snippets
- [ ] Decide if keeping Raycast for other features

### 2.3: Audio device switcher

**Files to create:**
- `~/.config/hammerspoon/audio-switcher.lua` - Audio output chooser

**Features:**
- Cmd+shift+space to show audio device chooser
- Display device name, mute status, volume
- One-key selection
- Auto-unmute on selection

**Testing:**
- Test with multiple audio devices
- Test mute/unmute behavior
- Test volume display accuracy

### 2.4: Auto-mute on wake

**Files to create:**
- `~/.config/hammerspoon/mute-on-sleep.lua` - System wake watcher

**Features:**
- Automatically mute audio on system wake
- Allowlist for specific devices (headphones, external DACs)
- Alert notification

**Configuration:**
- Define allowlisted devices (pattern matching)
- Customize alert behavior

**Testing:**
- Test with laptop speakers
- Test with allowlisted devices
- Test alert notifications

## Phase 3: Key remapping migration

### 3.1: Vim arrow keys (option+hjkl)

**Implementation:**
- Use `hs.hotkey.bind()` for global hotkeys
- Option+h → left arrow
- Option+j → down arrow
- Option+k → up arrow
- Option+l → right arrow

**Testing:**
- Test in various apps (browser, editor, terminal)
- Ensure no conflicts with app-specific shortcuts

### 3.2: Emacs-style navigation (ctrl+n/p)

**Implementation:**
- Ctrl+n → down arrow
- Ctrl+p → up arrow

**Testing:**
- Test in text fields, browsers, etc.
- Verify doesn't conflict with terminal apps

### 3.3: Hyperspace key (enter as hyper modifier)

**Implementation:**
- Tap enter → enter key
- Hold enter → hyper modifier (cmd+ctrl+alt+shift)
- 200ms threshold for detection

**Complexity note:**
- This requires `hs.eventtap` with complex timing logic
- Consider if worth migrating or keeping in Karabiner
- Karabiner handles this more reliably

**Decision point:**
- [ ] Migrate to Hammerspoon (more complex)
- [ ] Keep in Karabiner (simpler, more reliable)

**Recommendation:** Keep hyperspace in Karabiner, migrate simpler key remappings to Hammerspoon

### 3.4: Page navigation (cmd+shift+j/k)

**Implementation:**
- Cmd+shift+j → shift+page down
- Cmd+shift+k → shift+page up

**Testing:**
- Test in browsers and text editors
- Verify text selection behavior

### 3.5: Device-specific key swaps

**Current Karabiner config:**
- Apple Magic Keyboard: fn ↔ escape swap
- QMK keyboard: ignored (handled by QMK firmware)

**Decision:**
- Keep device-specific mappings in Karabiner
- Karabiner excels at device-specific rules

## Phase 4: Advanced features

### 4.1: Window switcher (AltTab replacement)

**Implementation options:**
1. `hs.window.switcher` - Built-in window switcher
2. `hs.chooser` - Custom UI with window list
3. Third-party spoon (WindowSigils, etc.)

**Features needed:**
- Show all windows across spaces
- Show app icons
- Keyboard navigation
- Preview on hover

**Testing:**
- Compare UX with AltTab
- Test with many windows open
- Test across multiple spaces

**Migration checklist:**
- [ ] Implement window switcher
- [ ] Configure keybinding (cmd+tab alternative?)
- [ ] Test for 1 week alongside AltTab
- [ ] Uninstall AltTab

### 4.2: Universal vim mode

**Decision:** Skip - not needed for this workflow

**Rationale:**
- VimMode.spoon is complex (~3000+ lines)
- Major workflow change with significant learning curve
- Not essential for productivity improvements
- Keep vim usage in terminal/editor where it belongs

**If reconsidered later:**
- Would require spoon workflow (too complex to inline)
- Add to versions.lua: `vimmode_spoon = "commit_hash"`
- Build and bundle via GitHub workflow

### 4.3: Monitor input switching

**Files to create:**
- `~/.config/hammerspoon/monitor-switching.lua` - DDC monitor control

**Requirements:**
- Install `ddcctl` via homebrew
- Configure monitor input values (USB-C, DisplayPort, HDMI)

**Features:**
- Hyper+w to cycle monitor inputs
- Useful for KVM-style setups

**Use case:**
- Only needed if sharing external monitors between computers
- Skip if not applicable

**Testing:**
- Test input switching
- Verify correct input values for your monitor

### 4.4: Custom automation ideas

**Additional possibilities to explore:**
- Automatic window layouts per space/desktop
- App-specific automation (Slack, Chrome, etc.)
- Clipboard history (if leaving Raycast)
- Caffeine replacement (prevent sleep)
- Menu bar integrations
- Time tracking automation
- Focus mode toggles
- Pomodoro timer

## Migration strategy

### Parallel running phase

**Duration:** 1-2 weeks per phase

**Approach:**
1. Implement Hammerspoon equivalent
2. Keep existing app running
3. Try to use Hammerspoon version by default
4. Fall back to old app if issues
5. Note any missing features or problems
6. Iterate on Hammerspoon config
7. When confident, disable/uninstall old app

### Testing checklist per feature

- [ ] Works in primary apps (browser, terminal, editor)
- [ ] No conflicts with app shortcuts
- [ ] Performance acceptable (no lag)
- [ ] Muscle memory adapting
- [ ] No missing features from replaced app
- [ ] Edge cases handled

### Rollback plan

All original apps remain installed during migration:
- Rectangle config backed up
- Karabiner config in git
- Raycast settings preserved
- Can re-enable any app if Hammerspoon fails

## Success criteria

### Must have
- Window management matches Rectangle functionality
- App switching is fast and reliable
- Key remapping works consistently
- Config is maintainable and documented

### Nice to have
- Fewer apps running in menu bar
- Unified configuration in Lua
- Custom automation possibilities unlocked
- Better understanding of macOS automation

### Deal breakers
- Noticeable lag or performance issues
- Reliability problems (crashes, missed keystrokes)
- Missing critical features from replaced apps
- Muscle memory taking too long to adapt

## File organization

All Hammerspoon config lives in `.config/hammerspoon/` (not `~/.hammerspoon/`):
- Consistent with other dotfile locations
- Easier to track in git
- Follows XDG-style conventions

Symlink from `~/.hammerspoon` → `.config/hammerspoon/` if needed

## Resources

### Documentation
- Hammerspoon API: https://www.hammerspoon.org/docs/
- dbalatero dotfiles: https://github.com/dbalatero/dotfiles/tree/main/hammerspoon (inspiration only)
- HyperKey spoon source: https://github.com/dbalatero/HyperKey.spoon (for inlining reference)

### Community
- Hammerspoon GitHub discussions
- /r/hammerspoon on Reddit
- Hammerspoon IRC channel

## Notes

- Hammerspoon is single-threaded, keep expensive operations fast
- Use `hs.timer.doAfter()` for async operations
- Always test after macOS updates (APIs can break)
- Keep backup of working config before major changes
- Consider splitting config into multiple files for maintainability
- Use `inspect = hs.inspect.inspect` for debugging
- Use `hs.hotkey.showHotkeys()` to see all bindings
