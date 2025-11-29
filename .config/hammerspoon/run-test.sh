#!/bin/bash
# Run Phase 2 test in Hammerspoon
# This will open the console and run the test

osascript <<EOF
tell application "Hammerspoon"
  execute lua code "dofile(hs.configdir .. '/test-phase2.lua')"
end tell
EOF
