#!/bin/sh
# Lists plugin names from nvim-pack-lock.json
# Outputs space-separated plugin names for make
# TODO: rewrite in lua once bootstrap provides lua at Makefile parse time
jq -r '.plugins | keys | join(" ")' .config/nvim/nvim-pack-lock.json
