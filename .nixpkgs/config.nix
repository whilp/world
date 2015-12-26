{ pkgs }: {
  allowUnfree = true;
  packageOverrides = super: let self = super.pkgs; in with self; rec {
  all = pkgs.buildEnv {
    name = "all";
    paths = [
      cacert
      diffutils
      aspell
      aspellDicts.en
      gnutls
      guile
      git
      # gitAndTools.hub EFFED
      go
      goPackages.godef
      goPackages.godep
      nix
      openssh
      m4
      autoconf
      automake
      gettext
      pkgconfig
      libtool
      clang
      libgcrypt
      gnupg
      gnumake
      racket # unsupported on darwin :/
      cloc
      libyaml
      ncurses
      readline
      python27Full
      # python27Packages.psycopg2
      (with python27Packages; [
        flake8
        virtualenv
        autopep8
        ipython
        readline
        sqlite3
        curses
      ])
      (emacsWithPackages
        (with emacsPackages; with emacsPackagesNg; [
          # ace-jump-zap
          # ace-link
          # ace-window
          # auto-indent-mode
          # browse-at-remote
          # clojure-mode
          # comment-dwim-2
          # company-go
          # company-nixos-options
          # corral
          # csv-mode
          # csv-nav
          # diff-hl
          # dockerfile-mode
          # easy-kill
          # edit-server-htmlize
          # eval-in-repl
          # go-eldoc
          # go-mode
          # go-oracle
          # graphviz-dot-mode
          # guru-mode
          # json-mode
          # lua-mode
          # nix-mode
          # ob-ipython
          # paredit
          # paredit-everywhere
          # pdf-tools
          # rcirc-color
          # rcirc-controls
          # restclient
          # unfill
          # which-key
          # yaml-mode
          async
          avy
          cask
          colorThemeSolarized
          company
          deferred
          expand-region
          flycheck
          gh
          gist
          gitModes
          htmlize
          js2
          magit
          markdown-mode
          org
          org-plus-contrib
          projectile
          scalaMode2
          smart-mode-line
          swiper
          use-package
      ]))
    ];
  };
};}
