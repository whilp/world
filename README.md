# 🏡 world

dotfiles extrêmes, tout reproductible, une maison modèle.

![maison modèle](https://brianlauritzen.files.wordpress.com/2013/05/house_fall.gif)

un monorepo contenant dotfiles, outils et trucs pour une informatique reproductible depuis 2005.

    $ git rev-list --max-parents=0 --pretty HEAD
    commit 165210edc61ec09e133b8e0af26d98e1e46de2ea
    Author: will <devnull@localhost>
    Date:   Sat Jun 11 21:26:33 2005 +0000

        [project @ 2005-06-11 16:26:33 by will]
        initial import into CVS

## versions

vous (moi) obtenez une archive-exécutable universelle qui fonctionne sur mac (arm64) et linux (arm64, x86_64 -- y compris le truc gvisor où vit claude.ai/code) avec dotfiles et outils (`home`). et *ça* sait récupérer des archives spécifiques à la plateforme avec le reste (outils pratiques).

la dernière version est à https://github.com/whilp/world/releases/latest

### yolo

```bash
curl -fsSL https://github.com/whilp/world/releases/latest/download/home | sh
```

### essayer lua

lua est disponible depuis [whilp/cosmopolitan](https://github.com/whilp/cosmopolitan/releases/latest) :

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

- [neovim](https://github.com/whilp/neovim) (fork de [neovim](https://neovim.io))
  - principalement pour figer une version
  - le build dans ce repo embarque neovim avec les plugins que j'utilise (au moment du build)
- [cosmopolitan](https://github.com/whilp/cosmopolitan) (fork de [cosmopolitan](https://justine.lol/cosmopolitan/)) - construit des exécutables vraiment portables :
  - `lua` avec toutes les bonnes choses de [`redbean`](https://redbean.dev) (`unix`, `path`, `re`, `sqlite3`, `argon2`, `json`, `cosmo`) plus des améliorations pour `Fetch` (support de proxy authentifiant). les modules lua purs sont zippés dans le binaire.
  - `make`, `zip`, `unzip` pour le bootstrapping

## licence

Ce dépôt est sous licence MIT. Vous êtes libre d'emprunter, utiliser ou adapter tout code ici pour vos propres projets. Voir le fichier [LICENSE](LICENSE) pour les détails.
