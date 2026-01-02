# 🏡 world

extreme dotfiles, all reproducible everything, a model home.

![model home](https://brianlauritzen.files.wordpress.com/2013/05/house_fall.gif)

a monorepo containing dotfiles, tools, and things for reproducible computering since 2005.

    $ git rev-list --max-parents=0 --pretty HEAD
    commit 165210edc61ec09e133b8e0af26d98e1e46de2ea
    Author: will <devnull@localhost>
    Date:   Sat Jun 11 21:26:33 2005 +0000

        [project @ 2005-06-11 16:26:33 by will]
        initial import into CVS

## releases

you (i) get a universal executable-archive that works on mac (arm64) and linux (arm64, x86_64 -- including the gvisor thingy that claude.ai/code lives in) with dotfiles and tools (`home`). and *that* knows how to fetch platform-specific archives with the rest (handy tools).

latest release is at https://github.com/whilp/world/releases/latest

### yolo

```bash
curl -fsSL https://github.com/whilp/world/releases/latest/download/home | sh
```

### try lua

lua is available from [whilp/cosmopolitan](https://github.com/whilp/cosmopolitan/releases/latest):

```console
$ curl -fsSLO https://github.com/whilp/cosmopolitan/releases/latest/download/lua && chmod +x lua
$ ./lua -e 'help()'
Cosmo Lua Help System

Modules:
  cosmo            - Encoding, hashing, compression, networking
  cosmo.unix       - POSIX system calls
  cosmo.path       - Path manipulation
  cosmo.re         - Regular expressions
  cosmo.lsqlite3   - SQLite database
  cosmo.argon2     - Password hashing

Usage:
  help()                      - This overview
  help("cosmo")               - List top-level functions
  help("cosmo.unix")          - List module contents
  help("cosmo.Fetch")         - Show function documentation
  help.search("base64")       - Search for matching functions

$ ./lua -e 'help("cosmo.Fetch")'
Fetch(url, body)

Sends an HTTP/HTTPS request to the specified URL. If only the URL is provided,
then a GET request is sent. If both URL and body parameters are specified, then
a POST request is sent. ...

Parameters:
  url (string)
  body (string|{) (optional): headers: table<string,string>, method: string, ...

Returns:
  integer: status, table<string,string> headers, string body
```

## forks

- [neovim](https://github.com/whilp/neovim) (fork of [neovim](https://neovim.io))
  - mostly to pin a snapshot
  - the build in this repo bundles neovim with the plugins i use (at buildtime)
- [cosmopolitan](https://github.com/whilp/cosmopolitan) (fork of [cosmopolitan](https://justine.lol/cosmopolitan/)) - builds actually portable executables:
  - `lua` with all the goodies from [`redbean`](https://redbean.dev) (`unix`, `path`, `re`, `sqlite3`, `argon2`, `json`, `cosmo`) plus enhancements for `Fetch` (authenticating proxy support). pure lua modules get zipped inside the binary.
  - `make`, `zip`, `unzip` for bootstrapping

## license

This repository is licensed under the MIT License. You are welcome to borrow, use, or adapt any code here for your own projects. See the [LICENSE](LICENSE) file for details.

