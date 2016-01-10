;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     spacemacs-ivy
     auto-completion
     better-defaults
     emacs-lisp
     git
     go
     osx
     python
     sql
     markdown
     yaml
     org
     (shell :variables
            shell-default-shell 'eshell
            shell-enable-smart-eshell t
            )
     spell-checking
     syntax-checking
     version-control
     rcirc
     whilp-rcirc
     whilp-projectile
     whilp-git
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(
                                      rich-minority
                                      comment-dwim-2
                                      flycheck-gometalinter
                                      golint
                                      )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(
                                    org-bullets
                                    powerline
                                    spaceline
                                    vi-tilde-fringe
                                    persp-mode
                                    )
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages nil))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'nil
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists nil
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         solarized-dark)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("pt" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  (setq user-full-name "Will Maier")
  (defvar user-email-address "wcmaier@gmail.com"
    "My email.")
  (setenv "GIT_EDITOR" "emacsclient")
  (setenv "GIT_COMMITTER_NAME" user-full-name)
  (setenv "GIT_COMMITTER_EMAIL" user-email-address)
  (setenv "GIT_AUTHOR_NAME" user-full-name)
  (setenv "GIT_AUTHOR_EMAIL" user-email-address)

  (defvar gopath (file-name-as-directory (expand-file-name "~/src"))
    "GOPATH.")
  (setenv "GOPATH" gopath)

  (defvar nix-link (file-name-as-directory (expand-file-name "~/.nix-profile"))
    "NIX_LINK.")

  (defvar nix-path (file-name-as-directory (expand-file-name "~/.nix-defexpr/nixpkgs")))

  (defvar ssl-cert-file (concat nix-link "etc/ssl/certs/ca-bundle.crt")
    "SSL_CERT_FILE.")

  (setq-default exec-path
                (mapcar
                 'expand-file-name
                 (list
                  (concat nix-link "bin")
                  (concat nix-link "sbin")
                  "~/bin"
                  ;; (concat whilp-gopath "bin")
                  "/usr/pkg/sbin"
                  "/usr/pkg/bin"
                  "/usr/local/sbin"
                  "/usr/local/bin"
                  "/usr/local/texlive/2015basic/bin/x86_64-darwin/"
                  "/usr/local/MacGPG2/bin"
                  "/usr/bin/"
                  "/bin/"
                  "/usr/sbin/"
                  "/sbin/"
                  "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9/"
                  "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9/")))

  (defvar shell-path (mapconcat 'identity exec-path path-separator)
    "Shell PATH string.")

  (setenv "NIX_PATH" (format "%s:nixpkgs=%s" nix-path nix-path))
  (setenv "NIX_CONF_DIR" (file-name-as-directory (expand-file-name "~/.nix")))
  (setenv "SSL_CERT_FILE" ssl-cert-file)
  (setenv "PATH" shell-path)

  (spacemacs|use-package-add-hook flyspell
    :pre-init (setenv "ASPELL_CONF" (format "dict-dir %slib/aspell" nix-link)))

  (spacemacs|use-package-add-hook comment-dwim-2
    :post-init (bind-keys ("M-;" . comment-dwim-2)))

  (spacemacs|use-package-add-hook window-numbering
    :post-config
    (bind-keys ("s-1" . select-window-1)
               ("s-2" . select-window-2)
               ("s-3" . select-window-3)
               ("s-4" . select-window-4)))

  (spacemacs|use-package-add-hook ido
    :post-config
    (setq ido-use-url-at-point t))

  (spacemacs|use-package-add-hook exec-path-from-shell
    :pre-init
    (setq exec-path-from-shell-variables '()))

  (require 'epa-file)
  (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$"
        epa-armor t)
  (epa-file-name-regexp-update)
  (epa-file-enable)

  (spacemacs|use-package-add-hook magit
    :post-config
    (setq magit-git-executable "git"))

  (spacemacs|use-package-add-hook sql
    :pre-init
    (setq sql-connection-alist
          `((redshift
             ,(sql-profile
               'postgres
               "prod-data-pipeline.cuxrn97vbxid.us-east-1.redshift.amazonaws.com"
               "will_20151109"
               5439)))
          ))

  (spacemacs|use-package-add-hook ace-link
    :post-config
    (when (fboundp #'ace-link-addr)
      (bind-keys ("M-o" . ace-link-addr))))

  (spacemacs|use-package-add-hook go-mode
    :post-config
    (setq gofmt-command "goimports"))

  (spacemacs|use-package-add-hook flycheck-gometalinter
    :pre-init
    (spacemacs|use-package-add-hook flycheck-mode-hook
      :post-config
      (add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup)))

  ;; go get -u github.com/golang/lint/golint
  ;; go get -u golang.org/x/tools/cmd/cover
  ;; go get -u golang.org/x/tools/cmd/oracle

  (setq ns-use-native-fullscreen nil)
  (global-visual-line-mode 1)
  (global-hl-line-mode -1)
  (add-hook 'text-mode-hook #'goto-address-mode)
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)

  ;; (global-evil-search-highlight-persist -1)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (setq rm-whitelist "nothin")
  (when (not rich-minority-mode)
    (rich-minority-mode 1))

  (add-hook 'before-save-hook 'whitespace-cleanup)

  (setq default-frame-alist '((fullscreen . fullscreen)
                              (vertical-scroll-bars)
                              (right-fringe . 4)
                              (left-fringe . 4)))

  (setq enable-local-variables nil
        enable-local-eval nil)

  (sp-use-paredit-bindings)
  (smartparens-global-strict-mode)

  (setq solarized-scale-org-headlines nil
        solarized-use-variable-pitch nil)
  (setq solarized-height-minus-1 1)
  (setq solarized-height-plus-1 1)
  (setq solarized-height-plus-2 1)
  (setq solarized-height-plus-3 1)
  (setq solarized-height-plus-4 1)

  (setq projectile-use-git-grep t)
  (setq org-use-speed-commands t
        org-speed-commands-user '(
                                  ("I" org-insert-heading-respect-content)
                                  ("s" call-interactively 'org-schedule)
                                  ("k" org-cut-subtree)
                                  ("w" org-copy-subtree)
                                  )
        org-startup-indented t
        org-enforce-todo-dependencies t
        org-return-follows-link t
        org-src-fontify-natively t
        org-completion-use-ido t
        org-return-follows-link t
        org-use-tag-inheritance t
        org-highest-priority ?A
        org-lowest-priority ?E
        org-default-priority ?A
        org-bookmark-names-plist '()
        org-use-fast-todo-selection nil
        org-default-notes-file "~/src/github.banksimple.com/whilp/notes/log.org"
        org-extend-today-until 6
        org-todo-keywords '((sequence "TODO" "|" "DONE" "PUNTED"))
        org-log-done 'note
        org-log-reschedule 'time
        org-log-redeadline 'time
        org-log-into-drawer "LOGBOOK")

  (defun browse-url-default-macosx-browser (url &optional new-window)
    "Browse URL in the background. (NEW-WINDOW is ignored)."
    (interactive (browse-url-interactive-arg "URL: "))
    (start-process (concat "open -g" url) nil "open" "-g" url))
  (global-evil-search-highlight-persist -1))

(defun spacemacs//restore-powerline (buffer)
  "Define a dummy restore-powerline.

configuration-layer/package-usedp returns t for powerline no matter what.")

(defun sql-profile (product host user port)
  "Search auth-info for an entry matching HOST, USER, and PORT."
  '(('sql-product product)
    ('sql-port port)
    ('sql-server host)
    ('sql-user user)
    ('sql-password (funcall
                   (plist-get
                    (car
                     (auth-source-search
                      :max 1
                      :host host
                      :user user
                      :port port
                      :create nil))
                    :secret)))))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
