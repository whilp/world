;;; init-whilp --- Emacs initialization

;;; Commentary:
;;; User configuration

;;; Code:

(require 'use-package)

;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; check out https://github.com/jwiegley/use-package
;; and do this: http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;; eval-in-repl https://github.com/kaz-yos/eval-in-repl
;; https://github.com/svend/emacs-essh
;; http://ess.r-project.org/
;; https://github.com/uk-ar/key-combo
;; https://github.com/capitaomorte/yasnippet
;; https://github.com/abo-abo/lispy
;; https://github.com/purcell/unfill

;; environment.

;; remember.
(winner-mode 1)
(desktop-save-mode 1)

(use-package savehist
  :demand t
  :config
  (progn
    (setq savehist-file "~/.emacs.d/savehist"
          savehist-additional-variables
          (append search-ring
                  regexp-search-ring))
    (savehist-mode 1)))

;; javascript
(use-package js
  :demand t
  :config (setq js-indent-level 2))

;; python
(use-package python
  :demand t
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i"))

(use-package minibuffer
  :demand t
  :config
  (setq completion-cycle-threshold 10))

;; I'm an adult.
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; parens trap.
(use-package paren
  :demand t
  :config
  (progn
    (setq show-paren-delay 0
          show-paren-style 'parenthesis)
    (show-paren-mode t)))

(global-hl-line-mode t)

;; comint.
(use-package comint
  :demand t
  :config
  (progn
    (setq comint-scroll-show-maximum-output nil)
    (remove-hook 'comint-output-filter-functions
                 'comint-postoutput-scroll-to-bottom)))


;; tramp.
(use-package tramp
  :demand t
  :config
  (progn
    (add-to-list 'tramp-default-proxies-alist
                 '(nil "\\`root\\'" "/ssh:%h:"))
    (add-to-list 'tramp-default-proxies-alist
                 '((regexp-quote (system-name)) nil nil))
    (setq tramp-password-prompt-regexp
          (concat "^.*"
                  (regexp-opt '("[pP]assword" "[pP]assphrase" "Verification code") t)
                  ".*:? *"))))

;; from @tom, to fix CM shiz
;; '(tramp-default-proxies-alist (quote ((nil "\\`root\\'" "/ssh:%h:"))))
;; '(tramp-ssh-controlmaster-options
;;  "-o ControlPath=%t.%%r@%%h:%%p -o ControlMaster=auto -o ControlPersist=no" t)
;; '(tramp-use-ssh-controlmaster-options nil)
;; 2015-02-12 18:29:50 <ieure> (eval-after-load "tramp" '(progn (setq tramp-use-ssh-controlmaster-options nil)))

(defun remote-shell (&optional host)
  "Open a remote shell to HOST."
  (interactive)
  (with-temp-buffer
    (let ((host (if host host (read-string "Host: "))))
      (cd (concat "/" host ":"))
      (shell (concat "*" host "*")))))

;; backups.
(setq make-backup-files nil
      auto-save-default nil
      backup-directory-alist `(("." . "~/.saves"))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" "~/.saves")))

;; pretty-print expression evals.
(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

;; windows.
(bind-keys ("s-SPC" . other-window)
           ("s-1" . delete-other-windows)
           ("s-2" . split-window-below)
           ("s-3" . split-window-right))

;; command-as-meta.
(setq mac-command-modifier 'meta
      mac-option-modifier 'super
      ;; TODO: these aren't defined in ns-win -- are they actually used anywhere?
      ;; mac-option-key-is-meta nil       
      ;; mac-command-key-is-meta t
      ns-function-modifier 'hyper
      kill-read-only-ok)

;; speling
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; whitespace.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(bind-keys ("<backtab>" . indent-relative))

;; wrap.
(global-visual-line-mode 1)

;; fix myself.
(use-package dabbrev
  :demand t
  :bind ("C-_" . dabbrev-expand)
  :config (setq abbrev-mode t))

;; nssh
(load-file "~/.emacs.d/nssh.el")

;; vc
(use-package compile
  :demand t
  :config
  (setq compilation-scroll-output t
      compilation-ask-about-save nil
      compilation-save-buffers-predicate '(lambda () nil)
))

(use-package ediff
  :demand t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package vc-hooks
  :demand t
  :config
  (bind-keys :map vc-prefix-map ("=" . ediff-revision)))

(cond
 ((string-equal system-type "darwin")
  (progn
    (setenv "GPG_AGENT_INFO" (expand-file-name "~/.gnupg/S.gpg-agent::1"))
    (setq epg-gpg-program "gpg2"))
  (set-face-attribute 'default nil :font "Monaco-16")))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-recipes")

(setq
 el-get-byte-compile t
 el-get-git-shallow-clone t
 el-get-user-package-directory "~/.emacs.d/el-get-init"
 el-get-sources '())

(el-get 'sync '(mu4e))

;; browse in the background.
(defun browse-url-default-macosx-browser (url &optional _new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (start-process (concat "open -g" url) nil "open" "-g" url))

(provide 'init-whilp)
;;; init-whilp ends here
