# 🏡 world

Extreme dotfiles, all reproducible everything, a model home.

![model home](https://brianlauritzen.files.wordpress.com/2013/05/bluths-model-home-falling.gif)

A monorepo containing dotfiles, tools, and infrastructure for a reproducible computing environment since 2005.

    $ git rev-list --max-parents=0 --pretty HEAD
    commit 165210edc61ec09e133b8e0af26d98e1e46de2ea
    Author: will <devnull@localhost>
    Date:   Sat Jun 11 21:26:33 2005 +0000

        [project @ 2005-06-11 16:26:33 by will]
        initial import into CVS

## Releases

Automated builds produce platform-specific installers for macOS (ARM64), Linux (ARM64, x86_64) containing dotfiles, tools, and configured third-party binaries.

[**Latest release →**](https://github.com/whilp/world/releases/latest)

### Quick setup

```bash
curl -fsSL https://github.com/whilp/world/releases/latest/download/home | sh
```

## Key components

- Custom [Neovim](https://github.com/whilp/neovim) builds with bundled plugins
- Custom [Cosmopolitan Libc](https://github.com/whilp/cosmopolitan) builds for portable binaries
- Dotfiles for zsh, git, and various development tools
- Lua-based configuration and build system
- Third-party binaries managed with checksum verification

## License

This repository is licensed under the MIT License. You are welcome to borrow, use, or adapt any code here for your own projects. See the [LICENSE](LICENSE) file for details.
