#!/usr/bin/env luajit

local posix = require('posix')

local function printf(fmt, ...)
  io.write(string.format(fmt, ...))
  io.flush()
end

local function errorf(fmt, ...)
  io.stderr:write(string.format(fmt, ...))
  io.stderr:flush()
  os.exit(1)
end

local function exec(cmd, opts)
  opts = opts or {}
  if not opts.quiet then
    printf("+ %s\n", cmd)
  end
  local ret = os.execute(cmd)
  if ret ~= 0 and not opts.allow_failure then
    errorf("command failed: %s\n", cmd)
  end
  return ret == 0
end

local function capture(cmd)
  local handle = io.popen(cmd)
  if not handle then
    errorf("failed to run: %s\n", cmd)
  end
  local result = handle:read("*a")
  handle:close()
  return result:gsub("%s+$", "")
end

local function mkdir_p(path)
  exec(string.format("mkdir -p %s", path), {quiet = true})
end

local function file_exists(path)
  local stat = posix.stat(path)
  return stat and stat.type == "regular"
end

local function read_file(path)
  local f = io.open(path, "r")
  if not f then
    errorf("failed to open file: %s\n", path)
  end
  local content = f:read("*a")
  f:close()
  return content
end

local function write_file(path, content)
  local f = io.open(path, "w")
  if not f then
    errorf("failed to open file for writing: %s\n", path)
  end
  f:write(content)
  f:close()
end

local function load_versions(versions_file)
  local versions = {}

  if file_exists(versions_file) then
    for line in io.lines(versions_file) do
      local key, value = line:match("^([^=]+)=(.+)$")
      if key and value then
        versions[key] = value
      end
    end
  end

  return versions
end

local function get_script_dir()
  local str = debug.getinfo(1, "S").source:sub(2)
  return str:match("^(.*/)") or "./"
end

local function detect_platform()
  local os_name = capture("uname -s"):lower()
  local arch = capture("uname -m")

  if arch == "aarch64" then
    arch = "arm64"
  elseif arch == "x86_64" then
    arch = "x64"
  end

  return {
    os = os_name,
    arch = arch,
    platform = os_name .. "-" .. arch,
    is_darwin = os_name == "darwin",
    is_linux = os_name == "linux",
  }
end

local function patch_lib_package(src_file, platform)
  printf("patching lib_package.c to support relocatable paths on unix...\n")

  local content = read_file(src_file)

  local includes_patch = [[
#if defined(__linux__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#include <limits.h>
#if defined(__APPLE__)
#include <mach-o/dyld.h>
#endif
#endif]]

  content = content:gsub(
    '(#include "lj_obj.h")',
    '%1\n' .. includes_patch
  )

  local setprogdir_impl = [[
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
#endif]]

  content = content:gsub(
    '#define setprogdir%(L%)%s+%(%s*%(void%)0%s*%)',
    setprogdir_impl
  )

  write_file(src_file, content)
end

local function patch_luaconf(conf_file, platform)
  printf("patching luaconf.h to use ! paths...\n")

  local content = read_file(conf_file)

  local lua_path_default = [[#define LUA_PATH_DEFAULT "./?.lua;!/../share/luajit-2.1/?.lua;!/../share/lua/5.1/?.lua;!/../share/lua/5.1/?/init.lua"]]

  local lua_cpath_default = [[#define LUA_CPATH_DEFAULT "./?.so;!/../lib/lua/5.1/?.so"]]

  content = content:gsub(
    '#define LUA_PATH_DEFAULT[^\n]*',
    lua_path_default
  )

  content = content:gsub(
    '#define LUA_CPATH_DEFAULT[^\n]*',
    lua_cpath_default
  )

  write_file(conf_file, content)
end

local function build_luajit(config)
  printf("building luajit %s for %s\n", config.version, config.platform.platform)
  printf("temporary directory: %s\n", config.temp_dir)
  printf("output directory: %s\n", config.output_dir)

  printf("cloning luajit repository...\n")
  local luajit_dir = config.temp_dir .. "/luajit"
  exec(string.format("git clone https://luajit.org/git/luajit.git %s", luajit_dir))

  printf("checking out %s...\n", config.version)
  exec(string.format("cd %s && git checkout %s", luajit_dir, config.version))

  local commit_hash = capture(string.format("cd %s && git rev-parse --short HEAD", luajit_dir))
  local commit_date = capture(string.format("cd %s && git log -1 --format=%%cd --date=format:%%Y.%%m.%%d", luajit_dir))
  local source_date_epoch = capture(string.format("cd %s && git log -1 --format=%%ct", luajit_dir))

  config.commit_hash = commit_hash
  config.build_date = commit_date
  config.source_date_epoch = source_date_epoch

  patch_lib_package(luajit_dir .. "/src/lib_package.c", config.platform)
  patch_luaconf(luajit_dir .. "/src/luaconf.h", config.platform)

  printf("building luajit...\n")
  local install_dir = config.temp_dir .. "/install"
  local build_env = string.format("SOURCE_DATE_EPOCH=%s TZ=UTC", source_date_epoch)

  if config.platform.is_darwin then
    build_env = build_env .. " MACOSX_DEPLOYMENT_TARGET=11.0"
  end

  local xcflags = "-DLUAJIT_ENABLE_GC64"
  local ldflags = ""

  if config.platform.is_linux then
    ldflags = "TARGET_LDFLAGS='-Wl,--build-id=none'"
  end

  exec(string.format(
    "cd %s && %s make amalg PREFIX=%s XCFLAGS='%s' %s",
    luajit_dir, build_env, install_dir, xcflags, ldflags
  ))

  printf("installing to temporary location...\n")
  exec(string.format("cd %s && TZ=UTC make install PREFIX=%s", luajit_dir, install_dir))

  printf("installing luarocks...\n")
  local luarocks_dir = config.temp_dir .. "/luarocks"
  exec(string.format("cd %s && git clone https://github.com/luarocks/luarocks.git", config.temp_dir))
  exec(string.format(
    "cd %s && ./configure --prefix=%s --with-lua=%s --lua-suffix=jit",
    luarocks_dir, install_dir, install_dir
  ))
  exec(string.format("cd %s && make && make install", luarocks_dir))

  local openssl_flags = ""
  local sqlite_flags = ""

  if config.platform.is_darwin then
    local openssl_prefix = capture("brew --prefix openssl@3")
    local sqlite_prefix = capture("brew --prefix sqlite")
    openssl_flags = string.format("OPENSSL_DIR=%s CRYPTO_DIR=%s", openssl_prefix, openssl_prefix)
    sqlite_flags = string.format(
      "SQLITE_DIR=%s SQLITE_INCDIR=%s/include SQLITE_LIBDIR=%s/lib",
      sqlite_prefix, sqlite_prefix, sqlite_prefix
    )
  else
    sqlite_flags = "SQLITE_INCDIR=/usr/include SQLITE_LIBDIR=/usr/lib"
  end

  printf("installing dependencies...\n")
  local luarocks = install_dir .. "/bin/luarocks"

  local deps = {
    {"luasocket", config.versions.LUASOCKET_VERSION, openssl_flags},
    {"luasec", config.versions.LUASEC_VERSION, openssl_flags},
    {"luaossl", config.versions.LUAOSSL_VERSION, openssl_flags},
    {"luaposix", config.versions.LUAPOSIX_VERSION, openssl_flags},
    {"dkjson", config.versions.DKJSON_VERSION, ""},
    {"luafilesystem", config.versions.LUAFILESYSTEM_VERSION, ""},
    {"compat53", config.versions.LUACOMPAT53_VERSION, ""},
    {"lpeg", config.versions.LPEG_VERSION, ""},
    {"lpeg_patterns", config.versions.LUALPEGPATTERNS_VERSION, ""},
    {"binaryheap", config.versions.LUABINARYHEAP_VERSION, ""},
    {"cqueues", config.versions.LUACQUEUES_VERSION, openssl_flags},
    {"http", config.versions.LUAHTTP_VERSION, ""},
    {"lsqlite3", config.versions.LSQLITE3_VERSION, sqlite_flags},
  }

  for _, dep in ipairs(deps) do
    local name, version, flags = dep[1], dep[2], dep[3]
    exec(string.format("%s install %s %s %s", luarocks, name, version, flags))
  end

  printf("fixing luarocks shebangs for relocatable installation...\n")
  for _, script in ipairs({"luarocks", "luarocks-admin"}) do
    local script_path = install_dir .. "/bin/" .. script
    if file_exists(script_path) then
      local content = read_file(script_path)
      content = content:gsub(
        "^#!" .. install_dir:gsub("[%-%.%+%[%]%(%)%$%^%%%?%*]", "%%%1") .. "/bin/luajit",
        "#!/usr/bin/env luajit"
      )
      write_file(script_path, content)
    end
  end

  printf("stripping binaries...\n")
  exec("find " .. install_dir .. " -type f -executable -exec strip --strip-unneeded {} \\; 2>/dev/null", {allow_failure = true})

  local platform_dir = config.output_dir .. "/" .. config.platform.platform
  mkdir_p(platform_dir)

  local binary_name = string.format("luajit-%s-%s-%s", config.build_date, config.commit_hash, config.platform.platform)
  local tarball_name = binary_name .. ".tar.gz"

  printf("packaging binary as %s...\n", tarball_name)

  local staging_dir = config.temp_dir .. "/staging"
  mkdir_p(staging_dir .. "/" .. binary_name)

  for _, dir in ipairs({"bin", "lib", "share", "include"}) do
    exec(string.format("cp -R %s/%s %s/%s/", install_dir, dir, staging_dir, binary_name))
  end

  local tarball_path = platform_dir .. "/" .. tarball_name

  if config.platform.is_darwin then
    exec(string.format(
      "cd %s && find %s -print0 | sort -z | tar -cf - --null -T - | gzip -n > %s",
      staging_dir, binary_name, tarball_path
    ))
  else
    exec(string.format(
      "cd %s && tar --sort=name --mtime='@%s' --owner=0 --group=0 --numeric-owner " ..
      "--pax-option=exthdr.name=%%d/PaxHeaders/%%f,delete=atime,delete=ctime " ..
      "-cf - %s | gzip -n > %s",
      staging_dir, source_date_epoch, binary_name, tarball_path
    ))
  end

  exec(string.format(
    "cd %s && ln -sf %s luajit-latest-%s.tar.gz",
    platform_dir, tarball_name, config.platform.platform
  ))

  printf("calculating sha256...\n")
  local checksum_cmd = "shasum -a 256"
  if not exec("command -v shasum >/dev/null 2>&1", {quiet = true, allow_failure = true}) then
    checksum_cmd = "sha256sum"
  end

  local checksum = capture(string.format(
    "cd %s && %s %s | cut -d' ' -f1",
    platform_dir, checksum_cmd, tarball_name
  ))

  write_file(
    platform_dir .. "/" .. tarball_name .. ".sha256",
    string.format("%s  %s\n", checksum, tarball_name)
  )

  printf("testing binary...\n")
  local luajit_bin = install_dir .. "/bin/luajit"
  exec(luajit_bin .. " -v")
  exec(luajit_bin .. " -e \"print('luajit test successful')\"")
  exec(luajit_bin .. " -e \"require('socket'); require('ssl'); print('luasocket and luasec loaded successfully')\"")
  exec(luajit_bin .. " -e \"require('openssl'); print('luaossl loaded successfully')\"")
  exec(luajit_bin .. " -e \"require('posix'); print('luaposix loaded successfully')\"")
  exec(luajit_bin .. " -e \"require('dkjson'); print('dkjson loaded successfully')\"")
  exec(luajit_bin .. " -e \"require('lfs'); print('luafilesystem loaded successfully')\"")
  exec(luajit_bin .. " -e \"require('http.client'); print('lua-http loaded successfully')\"")
  exec(luajit_bin .. " -e \"require('lsqlite3'); print('lsqlite3 loaded successfully')\"")

  local size = capture("du -h " .. luajit_bin .. " | cut -f1")
  printf("binary size: %s\n", size)

  if exec("command -v ldd >/dev/null 2>&1", {quiet = true, allow_failure = true}) then
    printf("dynamic libraries:\n")
    exec("ldd " .. luajit_bin, {allow_failure = true})
  end

  printf("cleaning up...\n")
  exec("rm -rf " .. config.temp_dir)

  printf("build complete!\n")
  printf("tarball: %s/%s\n", platform_dir, tarball_name)
  printf("sha256: %s\n", checksum)
end

local function main()
  local script_dir = get_script_dir()
  local versions_file = script_dir .. "../.config/luajit/versions.conf"

  local versions = load_versions(versions_file)

  local defaults = {
    LUAJIT_VERSION = "25a61a182166fec06f1a1a025eb8fabbb6cf483e",
    LUASOCKET_VERSION = "3.1.0-1",
    LUASEC_VERSION = "1.3.2-1",
    LUAOSSL_VERSION = "20250929-0",
    LUAPOSIX_VERSION = "36.3-1",
    DKJSON_VERSION = "2.8-1",
    LUAFILESYSTEM_VERSION = "1.8.0-1",
    LUACOMPAT53_VERSION = "0.14.4-1",
    LPEG_VERSION = "1.1.0-2",
    LUALPEGPATTERNS_VERSION = "0.5-0",
    LUABINARYHEAP_VERSION = "0.4-1",
    LUACQUEUES_VERSION = "20200726.51-0",
    LUAHTTP_VERSION = "0.4-0",
    LSQLITE3_VERSION = "0.9.6-1",
  }

  for k, v in pairs(defaults) do
    if not versions[k] then
      versions[k] = v
    end
  end

  local temp_dir = capture("mktemp -d")
  local output_dir = os.getenv("OUTPUT_DIR") or (script_dir .. "../dist/luajit")

  local config = {
    version = versions.LUAJIT_VERSION,
    versions = versions,
    temp_dir = temp_dir,
    output_dir = output_dir,
    platform = detect_platform(),
  }

  local ok, err = pcall(function()
    build_luajit(config)
  end)

  if not ok then
    errorf("build failed: %s\n", err)
  end
end

main()
