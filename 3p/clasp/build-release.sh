#!/bin/bash
# Build clasp binary for release
# Run this on a machine with npm access, then upload artifacts to GitHub releases
set -euo pipefail

CLASP_VERSION="${1:-3.1.3}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORK_DIR="$(mktemp -d)"
trap "rm -rf $WORK_DIR" EXIT

echo "Building clasp v${CLASP_VERSION}..."

# Download clasp source
curl -sL "https://github.com/google/clasp/archive/refs/tags/v${CLASP_VERSION}.tar.gz" | tar xz -C "$WORK_DIR"
cd "$WORK_DIR/clasp-${CLASP_VERSION}"

# Install dependencies
bun install --frozen-lockfile

# Build for current platform
PLATFORM="$(uname -s | tr '[:upper:]' '[:lower:]')-$(uname -m | sed 's/x86_64/x64/' | sed 's/aarch64/arm64/')"
OUTPUT="clasp-${CLASP_VERSION}-${PLATFORM}"

echo "Compiling for ${PLATFORM}..."
bun build --compile --minify src/index.ts --outfile "$OUTPUT"

# Move to output location
mkdir -p "$SCRIPT_DIR/dist"
mv "$OUTPUT" "$SCRIPT_DIR/dist/"

echo "Built: $SCRIPT_DIR/dist/$OUTPUT"
echo ""
echo "To create release artifacts for all platforms, run on each platform or use cross-compilation."
echo "Upload to: https://github.com/whilp/world/releases"
