---
name: cosmo-lua
description: Use cosmopolitan Lua (cosmo-lua) for portable scripts. Download from whilp/cosmopolitan releases. Includes unix syscalls, path utils, regex, sqlite, argon2.
allowed-tools: [Read, Write, Edit, Bash, Glob, Grep, WebFetch]
---

# Cosmo Lua

Portable Lua with batteries included from [whilp/cosmopolitan](https://github.com/whilp/cosmopolitan).

## Installation

Download the latest `lua` binary from [releases](https://github.com/whilp/cosmopolitan/releases):

```bash
curl -L -o lua https://github.com/whilp/cosmopolitan/releases/latest/download/lua
chmod +x lua
```

The binary runs on Linux, macOS, Windows, FreeBSD, OpenBSD, and NetBSD without dependencies.

## Update

Download the latest release and reinstall the skill:

```bash
curl -L -o lua https://github.com/whilp/cosmopolitan/releases/latest/download/lua
chmod +x lua
./lua --skill
```

## Built-in help

Use the interactive help system:

```lua
local help = require("cosmo.help")
help()                    -- overview
help("cosmo")             -- list module
help("cosmo.Fetch")       -- function docs
help.search("base64")     -- search
```

## Modules

- [cosmo](cosmo.md) - Encoding, hashing, compression, networking
- [cosmo.Context](cosmo-Context.md) - Context module
- [cosmo.Database](cosmo-Database.md) - Database module
- [cosmo.VM](cosmo-VM.md) - VM module
- [cosmo.argon2](cosmo-argon2.md) - Password hashing
- [cosmo.finger](cosmo-finger.md) - finger module
- [cosmo.lsqlite3](cosmo-lsqlite3.md) - lsqlite3 module
- [cosmo.maxmind](cosmo-maxmind.md) - maxmind module
- [cosmo.path](cosmo-path.md) - Path manipulation
- [cosmo.re](cosmo-re.md) - Regular expressions
- [cosmo.unix](cosmo-unix.md) - POSIX system calls
