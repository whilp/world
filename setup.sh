#!/bin/bash

SETUP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.config/setup" && pwd)"

main() {
  "$SETUP_DIR/backup"
  "$SETUP_DIR/git"
  "$SETUP_DIR/shell"
  "$SETUP_DIR/luajit"
  "$SETUP_DIR/shimlink"
  "$SETUP_DIR/claude" &
  "$SETUP_DIR/nvim" &
  "$SETUP_DIR/extras" &
  "$SETUP_DIR/ai" &
  wait
}

main "$@"
