#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VERSION="${LUAJIT_VERSION:-25a61a182166fec06f1a1a025eb8fabbb6cf483e}"
LUASOCKET_VERSION="${LUASOCKET_VERSION:-3.1.0-1}"
LUASEC_VERSION="${LUASEC_VERSION:-1.3.2-1}"
LUAOSSL_VERSION="${LUAOSSL_VERSION:-20250929-0}"
LUAPOSIX_VERSION="${LUAPOSIX_VERSION:-36.2.1-1}"
LUACJSON_VERSION="${LUACJSON_VERSION:-2.1.0.13-1}"
LUAFILESYSTEM_VERSION="${LUAFILESYSTEM_VERSION:-1.8.0-1}"
LUACOMPAT53_VERSION="${LUACOMPAT53_VERSION:-0.14-1}"
LUALPEGPATTERNS_VERSION="${LUALPEGPATTERNS_VERSION:-0.5-0}"
LUABINARYHEAP_VERSION="${LUABINARYHEAP_VERSION:-0.4-1}"
LUACQUEUES_VERSION="${LUACQUEUES_VERSION:-20200726.53-0}"
LUAHTTP_VERSION="${LUAHTTP_VERSION:-0.4-0}"
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

echo "Patching lib_package.c to support relocatable paths on Unix..."

# Detect sed variant for BSD/GNU compatibility
if [[ "${OS}" == "darwin" ]]; then
  SED_INPLACE=(-i '')
else
  SED_INPLACE=(-i)
fi

# We need to add includes and implementation before the setprogdir line
# First, add the necessary includes after the existing includes
sed "${SED_INPLACE[@]}" '/^#include "lj_obj.h"/a\
#if defined(__linux__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)\
#include <limits.h>\
#if defined(__APPLE__)\
#include <mach-o/dyld.h>\
#endif\
#endif' src/lib_package.c

# Now replace the setprogdir macro with the implementation
# Find the line number of "#define setprogdir(L)"
LINE_NUM=$(grep -n "^#define setprogdir(L)" src/lib_package.c | cut -d: -f1)

if [ -n "$LINE_NUM" ]; then
  # Create a temporary file with the replacement
  cat > /tmp/setprogdir_replacement.txt << 'REPLACEMENT'
#if defined(__linux__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
static void setprogdir(lua_State *L)
{
  char buff[PATH_MAX + 1];
  char *lb;
  ssize_t len = -1;

#if defined(__linux__)
  len = readlink("/proc/self/exe", buff, sizeof(buff) - 1);
#elif defined(__FreeBSD__)
  len = readlink("/proc/curproc/file", buff, sizeof(buff) - 1);
#elif defined(__NetBSD__)
  len = readlink("/proc/curproc/exe", buff, sizeof(buff) - 1);
#elif defined(__APPLE__)
  uint32_t size = sizeof(buff);
  if (_NSGetExecutablePath(buff, &size) == 0) {
    len = strlen(buff);
  }
#endif

  if (len > 0 && len < (ssize_t)sizeof(buff)) {
    buff[len] = '\0';
    lb = strrchr(buff, '/');
    if (lb != NULL) {
      *lb = '\0';
      luaL_gsub(L, lua_tostring(L, -1), LUA_EXECDIR, buff);
      lua_remove(L, -2);
    }
  }
}
#else
#define setprogdir(L) ((void)0)
#endif
REPLACEMENT

  # Replace the line with the content
  sed "${SED_INPLACE[@]}" "${LINE_NUM}r /tmp/setprogdir_replacement.txt" src/lib_package.c
  sed "${SED_INPLACE[@]}" "${LINE_NUM}d" src/lib_package.c
fi

echo "Patching luaconf.h to use ! paths..."
# Patch luaconf.h to use ! in the paths like Windows does
sed "${SED_INPLACE[@]}" '/^#define LUA_PATH_DEFAULT/c\
#define LUA_PATH_DEFAULT "./?.lua;!/../share/luajit-2.1/?.lua;!/../share/lua/5.1/?.lua;!/../share/lua/5.1/?/init.lua"
' src/luaconf.h

sed "${SED_INPLACE[@]}" '/^#define LUA_CPATH_DEFAULT/c\
#define LUA_CPATH_DEFAULT "./?.so;!/../lib/lua/5.1/?.so"
' src/luaconf.h

echo "Building LuaJIT..."
if [[ "${OS}" == "darwin" ]]; then
  make amalg PREFIX="${TEMP_DIR}/install" XCFLAGS="-DLUAJIT_ENABLE_GC64"
else
  make amalg PREFIX="${TEMP_DIR}/install" XCFLAGS="-DLUAJIT_ENABLE_GC64" TARGET_LDFLAGS="-Wl,--build-id=none"
fi

echo "Installing to temporary location..."
TZ=UTC make install PREFIX="${TEMP_DIR}/install"

echo "Installing LuaRocks..."
cd "${TEMP_DIR}"
git clone https://github.com/luarocks/luarocks.git
cd luarocks
./configure --prefix="${TEMP_DIR}/install" --with-lua="${TEMP_DIR}/install" --lua-suffix=jit
make
make install

echo "Installing dependencies..."
cd "${TEMP_DIR}/install"

if [[ "${OS}" == "darwin" ]]; then
  OPENSSL_FLAGS="OPENSSL_DIR=$(brew --prefix openssl@3) CRYPTO_DIR=$(brew --prefix openssl@3)"
else
  OPENSSL_FLAGS=""
fi

"${TEMP_DIR}/install/bin/luarocks" install luasocket ${LUASOCKET_VERSION} ${OPENSSL_FLAGS}
"${TEMP_DIR}/install/bin/luarocks" install luasec ${LUASEC_VERSION} ${OPENSSL_FLAGS}
"${TEMP_DIR}/install/bin/luarocks" install luaossl ${LUAOSSL_VERSION} ${OPENSSL_FLAGS}
"${TEMP_DIR}/install/bin/luarocks" install luaposix ${LUAPOSIX_VERSION} ${OPENSSL_FLAGS}
"${TEMP_DIR}/install/bin/luarocks" install lua-cjson ${LUACJSON_VERSION}
"${TEMP_DIR}/install/bin/luarocks" install luafilesystem ${LUAFILESYSTEM_VERSION}
"${TEMP_DIR}/install/bin/luarocks" install compat53 ${LUACOMPAT53_VERSION}
"${TEMP_DIR}/install/bin/luarocks" install lpeg_patterns ${LUALPEGPATTERNS_VERSION}
"${TEMP_DIR}/install/bin/luarocks" install binaryheap ${LUABINARYHEAP_VERSION}
"${TEMP_DIR}/install/bin/luarocks" install cqueues ${LUACQUEUES_VERSION} ${OPENSSL_FLAGS}
"${TEMP_DIR}/install/bin/luarocks" install http ${LUAHTTP_VERSION}

echo "Fixing luarocks shebangs for relocatable installation..."
for script in "${TEMP_DIR}/install/bin/luarocks" "${TEMP_DIR}/install/bin/luarocks-admin"; do
  if [ -f "$script" ]; then
    sed "${SED_INPLACE[@]}" "1s|#!${TEMP_DIR}/install/bin/luajit|#!/usr/bin/env luajit|" "$script"
  fi
done

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
./luajit -e "require('socket'); require('ssl'); print('LuaSocket and LuaSec loaded successfully')"
./luajit -e "require('openssl'); print('LuaOSSL loaded successfully')"
./luajit -e "require('posix'); print('LuaPosix loaded successfully')"
./luajit -e "require('cjson'); print('lua-cjson loaded successfully')"
./luajit -e "require('lfs'); print('luafilesystem loaded successfully')"
./luajit -e "require('http.client'); print('lua-http loaded successfully')"

echo "Binary size: $(du -h ./luajit | cut -f1)"
if command -v ldd >/dev/null 2>&1; then
  echo "Dynamic libraries:"
  ldd ./luajit || true
fi

echo "Cleaning up..."
rm -rf "${TEMP_DIR}"
