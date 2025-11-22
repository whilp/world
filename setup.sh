#!/bin/bash

SETUP_DIR="$HOME/.config/setup"

main() {
  "$SETUP_DIR/backup"
  "$SETUP_DIR/git"
  "$SETUP_DIR/shell"
  "$SETUP_DIR/luajit"
  "$SETUP_DIR/shimlink-luajit"
  "$SETUP_DIR/shimlink"
  "$SETUP_DIR/claude" &
  "$SETUP_DIR/nvim" &
  "$SETUP_DIR/extras" &
  "$SETUP_DIR/ai" &
  wait
}

main "$@"
