{ pkgs }: {
  allowUnfree = true;
  packageOverrides = super: let self = super.pkgs; in with self; rec {
  all = pkgs.buildEnv {
    name = "all";
    paths = [
      cacert
      diffutils
      (emacsWithPackages
        (with emacsPackages; with emacsPackagesNg; [
          # aspell
          # aspellDicts.en
          # abbrev
          # ace-jump-zap
          # ace-link
          # ace-window
          async
          # auto-indent-mode
          avy
          # battery
          # browse-at-remote
          # browse-url
          # clojure-mode
          colorThemeSolarized
          # comint
          # comment-dwim-2
          company
          # company-go
          # company-nixos-options
          # compile
          # corral
          # counsel
          # csv-mode
          # csv-nav
          # dabbrev
          deferred
          # desktop
          # diff-hl
          # dockerfile-mode
          # easy-kill
          # edify
          # edit-server-htmlize
          # eldoc
          # elec-pair
          # epa-file
          # eshell
          # eval-in-repl
          expand-region
          # files
          flycheck
          # flyspell
          # font-core
          # frame
          # fringe
          gh
          gist
          gitModes
          htmlize
          # go-eldoc
          # go-mode
          # go-oracle
          # graphviz-dot-mode
          # guru-mode
          # hl-line
          swiper # swiper ivy counsel
          # js
          js2
          # json-mode
          # lisp-mode
          # lua-mode
          magit
          markdown-mode
          # menu-bar
          # midnight
          # minibuffer
          # newcomment
          # nix-mode
          # ns-win
          # ob-ipython
          # ob-tangle
          org
          # org-agenda
          # org-capture
          # org-clock
          # org-context
          # org-mime
          org-plus-contrib
          # ox
          # ox-html
          # paredit
          # paredit-everywhere
          # pdf-tools
          # pp
          # prog-mode
          projectile
          # python
          # rcirc-color
          # rcirc-controls
          # restclient
          # ruby-mode
          # savehist
          # scala-mode2
          scalaMode2
          # scroll-bar
          # shell
          # simple
          smart-mode-line
          # time
          # tool-bar
          # tramp
          # unfill
          # uniquify
          use-package
          # vc-hooks
          # visual-fill-column
          # which-func
          # which-key
          # winner
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
