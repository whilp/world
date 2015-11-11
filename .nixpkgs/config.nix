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
          async
          # auto-indent-mode
          avy
          # browse-at-remote
          # clojure-mode
          colorThemeSolarized
          # comment-dwim-2
          company
          # company-go
          # company-nixos-options
          # corral
          # csv-mode
          # csv-nav
          deferred
          # diff-hl
          # dockerfile-mode
          # easy-kill
          # edit-server-htmlize
          # eval-in-repl
          expand-region
          flycheck
          gh
          gist
          gitModes
          htmlize
          # go-eldoc
          # go-mode
          # go-oracle
          # graphviz-dot-mode
          # guru-mode
          swiper # swiper ivy counsel
          js2
          # json-mode
          # lua-mode
          magit
          markdown-mode
          # nix-mode
          # ob-ipython
          org
          org-plus-contrib
          # paredit
          # paredit-everywhere
          # pdf-tools
          projectile
          # rcirc-color
          # rcirc-controls
          # restclient
          scalaMode2
          smart-mode-line
          # unfill
          use-package
          # which-key
          # yaml-mode
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
