;;; init --- Emacs initialization

;;; Commentary:

;;; My personal configuration.
;;; TODO:
;;;  - convert explicit (bind-keys) calls to :bind (after updating use-package)

;;; Code:

(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
;;             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'auth-source)
  (require 'cl)
  (require 'color-theme)
  (require 'url)
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package whilp-environment)
(use-package whilp-rcirc)
(use-package whilp-ui)

(use-package whilp-buffers
  :demand t
  :bind (("C-x C-b" . switch-to-buffer)
         ("s-b" . ibuffer)))

(use-package ibuffer-projectile
  :ensure t
  :defer t
  :config
  (progn
    (defun whilp-ibuffer-projectile ()
      "Add projectile buffers to `ibuffer-filter-groups`."
      (setq ibuffer-filter-groups
            (append
             ibuffer-filter-groups
             (ibuffer-projectile-generate-filter-groups)))
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))

    (add-hook 'ibuffer-hook 'whilp-ibuffer-projectile)))

(use-package projectile
  :ensure t
  :init (setq projectile-keymap-prefix (kbd "s-p"))
  :config
  (progn
    (bind-keys :map projectile-command-map
               ("f" . projectile-find-file)
               ("g" . projectile-vc-grep)
               ("!" . projectile-run-shell))

    (projectile-global-mode)

    (setq projectile-switch-project-action 'helm-projectile
          projectile-globally-ignored-directories
          (quote
           (
            ".idea"
            ".eunit"
            ".git"
            ".hg"
            ".fslckout"
            ".bzr"
            "_darcs"
            ".tox"
            ".svn"
            "build"
            "_workspace"))
          projectile-mode-line
          (quote
           (:eval (format " [%s]" (projectile-project-name)))))

    (defun projectile-run-shell (&optional buffer)
      "Start a shell in the project's root."
      (interactive "P")
      (projectile-with-default-dir (projectile-project-root)
        (shell (format "*shell %s*" (projectile-project-name)))))

    (defun projectile-vc-grep ()
      "Start a shell in the project's root."
      (interactive)
      (let ((regexp (if (and transient-mark-mode mark-active)
                        (buffer-substring (region-beginning) (region-end))
                      (read-string (projectile-prepend-project-name "Grep for: ")
                                   (projectile-symbol-at-point)))))
        (projectile-with-default-dir (projectile-project-root)
          (vc-git-grep regexp "\\*" (projectile-project-root)))))))

;; TODO
;; http://blog.danielgempesaw.com/post/79353633199/installing-mu4e-with-homebrew-with-emacs-from
;; (use-package mu4e
;;   :ensure t
;;   :defer t)

(use-package color-theme-solarized
  :ensure t
  :bind (("s-`" . whilp-toggle-solarized))
  :config
  (progn
    (require 'frame)
    (load-theme 'solarized t)
    (enable-theme 'solarized)

    (defun whilp-toggle-solarized ()
      "Toggles between solarized light and dark."
      (interactive)
      (let ((mode (if (equal (frame-parameter nil 'background-mode) 'dark) 'light 'dark)))
        (set-frame-parameter nil 'background-mode mode)
        (enable-theme 'solarized)))))

(use-package lispy
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))))

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind (("C-c SPC" . ace-jump-mode)))

(use-package deft
  :ensure t
  :defer t
  :bind (("s-d" . deft))
  :config
  (progn
    (setq deft-extension "txt"
          deft-directory "~/src/github.banksimple.com/whilp/notes"
          deft-text-mode 'org-mode
          deft-use-filename-as-title t)))

(use-package anaconda-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'eldoc-mode)))

(use-package cider
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :config
  (progn
    (global-company-mode)
    (bind-keys :map company-active-map
               ("\C-d" . company-show-doc-buffer)
               ("<tab>" . company-complete))
    (setq company-echo-delay 0
          company-tooltip-minimum-width 30
          company-idle-delay .7)))

(use-package company-go
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'go-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends) '(company-go))
                (company-mode)))))

(use-package company-restclient
  :ensure t
  :defer t)

(use-package company-inf-ruby
  :ensure t
  :defer t)

(use-package inf-ruby
  :ensure t
  :defer t
  :config
  (progn
    (setq ruby-deep-indent-paren nil
          ruby-deep-arglist nil)))

(use-package markdown-mode
  :ensure t
  :defer t
  :config (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package js2-mode
  :ensure t
  :defer t
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package org-mode
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c C-x C-o" . org-clock-out)
         ("C-c C-x C-x" . org-clock-in-last)
         ("C-c C-x C-i" . org-clock-in))
  :config
  (progn
    (setq auto-indent-start-org-indent t
          org-startup-indented t
          org-enforce-todo-dependencies t
          org-return-follows-link t
          org-src-fontify-natively t
          org-completion-use-ido t
          org-return-follows-link t
          org-use-tag-inheritance t
          org-agenda-start-on-weekday 1
          org-agenda-dim-blocked-tasks t
          org-todo-keywords '((sequence "TODO" "DONE"))
          org-agenda-skip-scheduled-if-done t
          org-agenda-restore-windows-after-quit nil
          org-agenda-window-setup 'current-window
          org-agenda-search-headline-for-time nil
          org-extend-today-until 6
          org-agenda-start-with-follow-mode nil
          org-agenda-todo-ignore-deadlines 'past
          org-agenda-todo-ignore-with-date t
          org-agenda-skip-scheduled-if-deadline-is-shown t
          org-agenda-todo-ignore-timestamp 'all
          org-agenda-todo-ignore-scheduled 'all
          org-agenda-skip-deadline-if-done t
          org-agenda-tags-todo-honor-ignore-options t
          org-agenda-follow-mode nil
          org-agenda-file-regexp "\\`[^.].*\\.\\(txt\\|org\\)\\'"
          org-agenda-files '("~/src/github.banksimple.com/whilp/notes/plan.txt")
          org-bookmark-names-plist '()
          org-default-notes-file "~/notes/todo.txt"
          org-clock-mode-line-total 'today
          org-clock-history-length 50
          org-agenda-repeating-timestamp-show-all nil
          org-log-into-drawer "LOGBOOK")

    (add-to-list 'auto-mode-alist '("\\.\\(txt\\|org\\)$" . org-mode))
    (setq org-capture-templates
          '(("t" "Todo" entry (file "~/src/github.banksimple.com/whilp/notes/plan.txt")
             "* TODO %?\nDEADLINE: %^t")))))

;; TODO
;; (use-package go-oracle)

(use-package go-mode
  :ensure t
  :defer t
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'go-mode-hook (lambda ()
                              (local-set-key (kbd "M-.") #'godef-jump)))))

(use-package deferred
  :ensure t
  :defer t)

(use-package async
  :ensure t
  :config
  (progn
    (require 'smtpmail-async)
    (setq ;;send-mail-function 'smtpmail-send-it
          ;;message-send-mail-function 'message-send-mail-with-sendmail
          send-mail-function 'async-smtpmail-send-it
          message-send-mail-function 'async-smtpmail-send-it
          )))

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-=" . er/expand-region)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x))
  :config
  (progn 
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))
    (setq helm-split-window-in-side-p t
          helm-move-to-line-cycle-in-source t
          helm-ff-search-library-in-sexp t
          helm-scroll-amount 8
          helm-ff-file-name-history-use-recentf t)
    (helm-mode 1)))

(use-package helm-projectile
  :ensure t
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))

(use-package helm-swoop
  :ensure t
  :bind (("M-s o" . helm-swoop)))

(use-package rcirc-color
  :ensure t
  :config
  (progn
    (eval-after-load 'rcirc '(require 'rcirc-color))
    (setq rcirc-color-is-deterministic t)))

(use-package rcirc-controls
  :ensure t)

(use-package restclient
  :ensure t
  :defer t)

(use-package minitest
  :ensure t
  :defer t)

(use-package scala-mode2
  :ensure t
  :defer t)

(use-package robe
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)
    (eval-after-load 'company
      '(push 'company-robe company-backends))))

(use-package rubocop
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'ruby-mode-hook 'rubocop-mode)
    (eval-after-load 'flycheck
      (flycheck-add-next-checker 'chef-foodcritic 'ruby-rubocop))))

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (sml/setup)
    (setq sml/mode-width 'right
          sml/shorten-directory t
          sml/use-projectile-p nil
          sml/full-mode-string ""
          sml/shorten-mode-string ""
          sml/name-width '(12 . 18))

    (setq-default global-mode-string
                  '(
                    ""
                    emms-mode-line-string
                    emms-playing-time-string
                    erc-modified-channels-object)
                  mode-line-format
                  '(
                    "%e"
                    display-time-string
                    mode-line-front-space
                    mode-line-mule-info
                    mode-line-client
                    mode-line-remote
                    mode-line-frame-identification
                    mode-line-buffer-identification
                    (vc-mode vc-mode)
                    "  " mode-line-modes
                    mode-line-misc-info
                    mode-line-end-spaces))
    ))

(use-package git-gutter-fringe+
  :ensure t
  :config
  (progn
    (bind-keys :map git-gutter+-mode-map
               ("C-x n" . git-gutter+-next-hunk)
               ("C-x p" . git-gutter+-previous-hunk)
               ("C-x v =" . git-gutter+-show-hunk)
               ("C-x r" . git-gutter+-revert-hunks)
               ("C-x t" . git-gutter+-stage-hunks)
               ("C-x c" . git-gutter+-commit)
               ("C-x C" . git-gutter+-stage-and-commit)
               ("C-x C-y" . git-gutter+-stage-and-commit-whole-buffer)
               ("C-x U" . git-gutter+-unstage-whole-buffer))

    (global-git-gutter+-mode t)
    (git-gutter-fr+-minimal)
    (setq git-gutter+-lighter "")))

(use-package go-eldoc
  :ensure t
  :defer t
  :config ((add-hook 'go-mode-hook 'go-eldoc-setup)))

(use-package gh
  :ensure t
  :defer t
  :config
  (progn
    (defun* whilp-gh-profile (url user)
      (let* (
             (urlobj (url-generic-parse-url url))
             (host (url-host urlobj))
             (auth-info
              (car
               (auth-source-search
                :max 1
                :host host
                :user user
                :port 443
                :create nil)))
             (token (funcall (plist-get auth-info :secret))))
        (list
         :url url
         :username user
         :token token
         :remote-regexp (gh-profile-remote-regexp host))))

    (setq
     gh-profile-default-profile "bh"
     gh-profile-current-profile nil
     gh-profile-alist
     (list
      (cons "bh" (whilp-gh-profile "https://github.banksimple.com/api/v3" "whilp"))
      (cons "gh" (whilp-gh-profile "https://api.github.com" "whilp"))))))

(use-package gist
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package git-link
  :ensure t
  :defer t)

(use-package ace-jump-buffer
  :ensure t
  :defer t)

(use-package ace-jump-mode
  :ensure t
  :defer t)

(use-package ace-jump-zap
  :ensure t
  :defer t)
(use-package magit
  :ensure t
  :diminish magit-auto-revert-mode
  :config
  (progn
    (bind-keys :prefix-map whilp-magit-map
               :prefix "s-g"
               ("g" . magit-status)
               ("u" . magit-push)
               ("p" . magit-grep)
               ("c" . magit-commit))
    (autoload 'magit-status' "magit" nil t)
    (setq magit-git-executable "gh"
          magit-save-some-buffers nil
          magit-status-buffer-switch-function 'switch-to-buffer
          magit-set-upstream-on-push 'dontask
          magit-completing-read-function 'magit-builtin-completing-read
          magit-last-seen-setup-instructions "1.4.0"
          magit-use-overlays nil)))
  
(use-package yasnippet
  :ensure t
  :defer 60
  :diminish yas-minor-mode
  :config (yas-global-mode))

(use-package flycheck
  :ensure t
  :config
  (progn
    (setq-default flycheck-emacs-lisp-load-path load-path)
    (global-flycheck-mode)))

(use-package epa-file
  :config
  (progn
    (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$"
          epa-armor t)
    (epa-file-name-regexp-update)
    (epa-file-enable)))

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config (golden-ratio-mode))

(use-package flyspell
  :diminish flyspell-mode)

(use-package simple
  :diminish visual-line-mode)

(use-package abbrev
  :diminish abbrev-mode)

;; TODO: migrate all these bits to separate libraries.
(require 'init-whilp)

(provide 'init)
;;; init ends here
