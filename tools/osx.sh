#!/bin/bash

set -euo pipefail

main() {
    defaults write com.apple.screencapture location -string ~/Documents/Screenshots
    # https://www.defaults-write.com/speed-up-macos-high-sierra/
    defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false
    defaults write -g QLPanelAnimationDuration -float 0
    defaults write NSGlobalDomain NSWindowResizeTime -float 0.001
    defaults write com.apple.finder DisableAllAnimations -bool true
    defaults write com.apple.dock launchanim -bool false
    defaults write com.apple.dock expose-animation-duration -float 0.1
    defaults write com.apple.Dock autohide-delay -float 0

    # https://github.com/mathiasbynens/dotfiles/issues/711
    defaults write com.apple.universalaccess reduceMotion -bool true

    defaults write com.google.Chrome AppleEnableSwipeNavigateWithScrolls -bool FALSE

    echo "Enable Reduce Motion preference"
}

main "$@"
exit $?
