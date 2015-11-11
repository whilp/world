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
      git
      gitAndTools.hub
      go
      goPackages.godef
      goPackages.godep
      nix
      openssh
    ];
  };
};}
