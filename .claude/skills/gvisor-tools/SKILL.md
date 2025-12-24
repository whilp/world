---
name: gvisor-tools
description: Set up Lua, Make, and Curl in gVisor environments like claude.ai/code. Use when the user needs build tools, Lua scripting, or curl in a sandboxed environment where standard APE binaries don't work.
allowed-tools: [Bash, Read, Write, Edit]
---

# gVisor tools

Set up and use Lua, Make, Curl, and other build tools in gVisor-based sandbox environments like claude.ai/code.

## When to use

Use this skill when:
- Working in claude.ai/code or other gVisor environments
- Need to run Lua scripts with cosmo.* modules
- Need GNU Make for build automation
- Need curl for HTTP requests
- Standard Cosmopolitan APE binaries fail with exec errors

## Setup

Download and extract the assimilated tools:

```bash
# Download latest release
curl -fsSL "https://github.com/whilp/dotfiles/releases/latest/download/assimilated-linux-x86_64.tar.gz" -o /tmp/assimilated.tar.gz

# Extract to home directory
tar -xzf /tmp/assimilated.tar.gz -C ~

# Set up environment (add to shell profile for persistence)
source ~/gvisor/env.sh
```

Or as a one-liner:

```bash
curl -fsSL "https://github.com/whilp/dotfiles/releases/latest/download/assimilated-linux-x86_64.tar.gz" | tar -xz -C ~ && source ~/gvisor/env.sh
```

## Available tools

After setup, these tools are available:

| Tool | Description |
|------|-------------|
| `lua` | Lua 5.4 with cosmo.* modules built-in |
| `make` | GNU Make 4.4.1 |
| `curl` | curl with TLS support |
| `zip` | Info-ZIP compression |
| `unzip` | Info-ZIP extraction |

## Lua modules

The Lua interpreter includes these **compiled-in** modules (no LUA_PATH needed):

- `cosmo` - Main module with encoding, compression, hashing utilities
- `cosmo.unix` - Unix system calls (fork, exec, pipe, signals)
- `cosmo.path` - Path manipulation (dirname, basename, join)
- `cosmo.re` - POSIX regular expressions
- `cosmo.argon2` - Argon2 password hashing
- `cosmo.sqlite3` - SQLite3 database

These modules are available via LUA_PATH (set by env.sh):

- `luaunit` - Unit testing framework
- `luacheck` - Lua linter
- `argparse` - Command-line argument parser
- `lfs` - Filesystem operations (stub implementation)

## Usage examples

### Run Lua with cosmo modules

```bash
lua -e 'print(require("cosmo").GetHostOs())'
# Output: LINUX

lua -e '
local unix = require("cosmo").unix
print("PID:", unix.getpid())
print("CWD:", unix.getcwd())
'
```

### Run luacheck

```bash
lua ~/gvisor/lib/lua/bin/luacheck myfile.lua
```

### Use Make

```bash
make --version
make -f Makefile build
```

### HTTP requests with curl

```bash
curl -fsSL https://example.com/api/data
curl -X POST -d '{"key": "value"}' https://api.example.com/endpoint
```

### SQLite in Lua

```lua
local sqlite3 = require("cosmo").sqlite3
local db = sqlite3.open(":memory:")

db:exec[[
  CREATE TABLE test (id INTEGER PRIMARY KEY, name TEXT);
  INSERT INTO test VALUES (1, 'hello');
]]

for row in db:nrows("SELECT * FROM test") do
  print(row.id, row.name)
end

db:close()
```

### Subprocess execution

```lua
local unix = require("cosmo").unix

-- Fork and exec
local pid = unix.fork()
if pid == 0 then
  unix.execve("/usr/bin/echo", {"echo", "hello from child"}, unix.environ())
  unix.exit(1)
else
  unix.wait(pid)
end
```

## Verification

After setup, verify the tools work:

```bash
# Check versions
lua -v
make --version | head -1
curl --version | head -1

# Verify cosmo module
lua -e 'print("OS:", require("cosmo").GetHostOs())'

# Verify external modules
lua -e 'print("luaunit:", require("luaunit"))'
```

## Troubleshooting

### "exec format error"

This means you're trying to run an APE (Actually Portable Executable) binary in gVisor. Use the assimilated binaries from this toolkit instead.

### Module not found

Ensure env.sh was sourced:

```bash
source ~/gvisor/env.sh
echo $LUA_PATH
```

### Permission denied

Make binaries executable:

```bash
chmod +x ~/gvisor/bin/*
```

## Technical details

These are native ELF binaries created by assimilating Cosmopolitan APE binaries. They're statically linked and have no external dependencies, making them ideal for minimal container environments.

The Lua binary has cosmo.* modules compiled in (they survive assimilation). Luaunit/luacheck are loaded from the filesystem via LUA_PATH.
