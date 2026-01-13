#!/bin/sh
# clasp wrapper - runs clasp from staged source with bun
set -e

BUN="__BUN__"
CLASP_DIR="__CLASP_DIR__"

# install deps on first run
if [ ! -d "$CLASP_DIR/node_modules" ]; then
  echo "Installing clasp dependencies..." >&2
  (cd "$CLASP_DIR" && "$BUN" install --frozen-lockfile 2>/dev/null || "$BUN" install)
fi

exec "$BUN" run "$CLASP_DIR/src/index.ts" "$@"
