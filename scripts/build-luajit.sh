#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VERSION="${LUAJIT_VERSION:-25a61a182166fec06f1a1a025eb8fabbb6cf483e}"
TEMP_DIR=$(mktemp -d)
OUTPUT_DIR="${OUTPUT_DIR:-${SCRIPT_DIR}/../dist/luajit}"

OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

case "${ARCH}" in
aarch64)
  ARCH="arm64"
  ;;
x86_64)
  ARCH="x64"
  ;;
esac

PLATFORM="${OS}-${ARCH}"

echo "Building LuaJIT ${VERSION} for ${PLATFORM}"
echo "Temporary directory: ${TEMP_DIR}"
echo "Output directory: ${OUTPUT_DIR}"

echo "Cloning LuaJIT repository..."
git clone https://luajit.org/git/luajit.git "${TEMP_DIR}/luajit"
cd "${TEMP_DIR}/luajit"

echo "Checking out ${VERSION}..."
git checkout "${VERSION}"

COMMIT_HASH=$(git rev-parse --short HEAD)
COMMIT_DATE=$(git log -1 --format=%cd --date=format:%Y.%m.%d)
BUILD_DATE="${COMMIT_DATE}"

export SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)
export TZ=UTC

if [[ "${OS}" == "darwin" ]]; then
  export MACOSX_DEPLOYMENT_TARGET=11.0
fi

echo "Building LuaJIT..."
if [[ "${OS}" == "darwin" ]]; then
  make amalg PREFIX="${TEMP_DIR}/install" XCFLAGS="-DLUAJIT_ENABLE_GC64"
else
  make amalg PREFIX="${TEMP_DIR}/install" XCFLAGS="-DLUAJIT_ENABLE_GC64" TARGET_LDFLAGS="-Wl,--build-id=none"
fi

echo "Installing to temporary location..."
TZ=UTC make install PREFIX="${TEMP_DIR}/install"

echo "Stripping binaries..."
find "${TEMP_DIR}/install" -type f -executable -exec strip --strip-unneeded {} \; 2>/dev/null || true

mkdir -p "${OUTPUT_DIR}/${PLATFORM}"

BINARY_NAME="luajit-${BUILD_DATE}-${COMMIT_HASH}-${PLATFORM}"
TARBALL_NAME="${BINARY_NAME}.tar.gz"

echo "Packaging binary as ${TARBALL_NAME}..."
cd "${TEMP_DIR}/install"

# Create a temporary directory with the correct structure for tarball
STAGING_DIR="${TEMP_DIR}/staging"
mkdir -p "${STAGING_DIR}/${BINARY_NAME}"

# Copy files to staging directory
cp -R bin "${STAGING_DIR}/${BINARY_NAME}/"
cp -R lib "${STAGING_DIR}/${BINARY_NAME}/"
cp -R share "${STAGING_DIR}/${BINARY_NAME}/"
cp -R include "${STAGING_DIR}/${BINARY_NAME}/"

# Create tarball with options compatible with both GNU and BSD tar
cd "${STAGING_DIR}"
if [[ "${OS}" == "darwin" ]]; then
  # BSD tar on macOS
  find "${BINARY_NAME}" -print0 | sort -z | tar -cf - --null -T - | gzip -n >"${OUTPUT_DIR}/${PLATFORM}/${TARBALL_NAME}"
else
  # GNU tar on Linux
  tar --sort=name \
    --mtime="@${SOURCE_DATE_EPOCH}" \
    --owner=0 \
    --group=0 \
    --numeric-owner \
    --pax-option=exthdr.name=%d/PaxHeaders/%f,delete=atime,delete=ctime \
    -cf - \
    "${BINARY_NAME}" |
    gzip -n >"${OUTPUT_DIR}/${PLATFORM}/${TARBALL_NAME}"
fi

cd "${OUTPUT_DIR}/${PLATFORM}"
ln -sf "${TARBALL_NAME}" "luajit-latest-${PLATFORM}.tar.gz"

echo "Calculating SHA256..."
if command -v shasum >/dev/null 2>&1; then
  CHECKSUM=$(shasum -a 256 "${TARBALL_NAME}" | cut -d' ' -f1)
else
  CHECKSUM=$(sha256sum "${TARBALL_NAME}" | cut -d' ' -f1)
fi

echo "${CHECKSUM}  ${TARBALL_NAME}" >"${TARBALL_NAME}.sha256"

echo "Testing binary..."
cd "${TEMP_DIR}/install/bin"
./luajit -v
./luajit -e "print('LuaJIT test successful')"

echo "Binary size: $(du -h ./luajit | cut -f1)"
if command -v ldd >/dev/null 2>&1; then
  echo "Dynamic libraries:"
  ldd ./luajit || true
fi

echo "Cleaning up..."
rm -rf "${TEMP_DIR}"
