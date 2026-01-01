#!/bin/sh
# Lists plugin names from nvim-pack-lock.json
# Outputs space-separated plugin names for make
jq -r '.plugins | keys | join(" ")' .config/nvim/nvim-pack-lock.json
