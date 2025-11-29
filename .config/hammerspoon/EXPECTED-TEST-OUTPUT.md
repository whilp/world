# Expected test output

When running `test-phase2.lua`, you should see output similar to this:

```
================================================================================
  Phase 2 implementation test
================================================================================

Loading modules...
  ✓ clue-loader loaded
  ✓ clue-manager loaded

--------------------------------------------------------------------------------
  Loading clue files
--------------------------------------------------------------------------------
  ✓ All clue files loaded successfully

--------------------------------------------------------------------------------
  Clue counts by group
--------------------------------------------------------------------------------
  cleanshot: 9 clues
  system: 13 clues
  hammerspoon: 5 clues
  test: 3 clues

--------------------------------------------------------------------------------
  Validating clue counts
--------------------------------------------------------------------------------
  ✓ cleanshot group has 9 clues
  ✓ system group has 13 clues
  ✓ hammerspoon group has 5 clues
  ✓ test group has 3 clues
  ✓ total clues equals 30

--------------------------------------------------------------------------------
  Modal generation
--------------------------------------------------------------------------------
  Total modals generated: 4

  ✓ exactly 4 modals generated
  ✓ hyper:c modal exists
  ✓ hyper:s modal exists
  ✓ hyper:h modal exists
  ✓ hyper:t modal exists

--------------------------------------------------------------------------------
  Modal structure validation
--------------------------------------------------------------------------------
  Modal: hyper:c
  ✓ hyper:c has trigger
  ✓ hyper:c has bindings
  ✓ hyper:c trigger is table
  ✓ hyper:c trigger has 2 elements
    Trigger: hyper + c
    Bindings: 9

  Modal: hyper:h
  ✓ hyper:h has trigger
  ✓ hyper:h has bindings
  ✓ hyper:h trigger is table
  ✓ hyper:h trigger has 2 elements
    Trigger: hyper + h
    Bindings: 5

  Modal: hyper:s
  ✓ hyper:s has trigger
  ✓ hyper:s has bindings
  ✓ hyper:s trigger is table
  ✓ hyper:s trigger has 2 elements
    Trigger: hyper + s
    Bindings: 13

  Modal: hyper:t
  ✓ hyper:t has trigger
  ✓ hyper:t has bindings
  ✓ hyper:t trigger is table
  ✓ hyper:t trigger has 2 elements
    Trigger: hyper + t
    Bindings: 3

--------------------------------------------------------------------------------
  Detailed modal inspection
--------------------------------------------------------------------------------
Modal: hyper:c
  Trigger: [hyper, c]
  Bindings: 9
  ✓ hyper:c has expected binding count
  Keys:
    a → All-in-one capture
    c → Capture area
    d → Toggle desktop icons
    f → Capture fullscreen
    h → Open capture history
    o → OCR text from screen
    r → Record screen
    s → Capture scrolling window
    w → Capture window

Modal: hyper:s
  Trigger: [hyper, s]
  Bindings: 13
  ✓ hyper:s has expected binding count
  Keys:
    a → Accessibility
    b → Battery
    d → Displays
    g → General
    i → Notifications
    k → Keyboard
    l → Appearance
    n → Network
    o → Sound
    p → Privacy & Security
    s → System Settings
    t → Trackpad
    w → Desktop & Dock

Modal: hyper:h
  Trigger: [hyper, h]
  Bindings: 5
  ✓ hyper:h has expected binding count
  Keys:
    c → Console
    e → Emoji picker
    r → Reload config
    s → Symbol picker
    u → Update apps

Modal: hyper:t
  Trigger: [hyper, t]
  Bindings: 3
  ✓ hyper:t has expected binding count
  Keys:
    a → Test alert
    c → Test console
    n → Test notify

--------------------------------------------------------------------------------
  Clue ID validation
--------------------------------------------------------------------------------
  ✓ all clues have IDs
  ✓ all clue IDs are strings
  ✓ clue IDs match expected format
  ✓ no duplicate clue IDs

--------------------------------------------------------------------------------
  Clue structure validation
--------------------------------------------------------------------------------
  ✓ all clues have name
  ✓ all clues have key
  ✓ all clues have action
  ✓ all clue keys are tables
  ✓ all clue keys have 3 elements
  ✓ all clue actions are tables
  ✓ all clue actions have exactly one action type

--------------------------------------------------------------------------------
  Group membership validation
--------------------------------------------------------------------------------
  ✓ all clues belong to a group
  ✓ all groups are valid
  ✓ group lists match clue group assignments

================================================================================

================================================================================
  Test summary
================================================================================
  Total tests: 46
  Passed: 46
  Warnings: 0
  Errors: 0

================================================================================
✓ Phase 2 implementation validated successfully!
================================================================================

true
```

## Key things to verify

1. All 30 clues load successfully
2. All 4 modals are generated (hyper:c, hyper:s, hyper:h, hyper:t)
3. Each modal has the correct number of bindings:
   - hyper:c: 9 bindings
   - hyper:s: 13 bindings
   - hyper:h: 5 bindings
   - hyper:t: 3 bindings
4. All clue IDs are unique and properly formatted
5. All structural validations pass
6. Test returns `true` indicating success
