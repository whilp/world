;;; init-whilp --- Emacs initialization

;;; Commentary:
;;; User configuration

;;; Code:

(require 'use-package)
(require 'browse-url)

;; remember.
(use-package desktop
  :demand t
  :config (desktop-save-mode 1))

(use-package savehist
  :demand t
  :config
  (progn
    (setq savehist-file "~/.emacs.d/savehist"
          savehist-additional-variables
          (mapcar 'make-symbol
                  (append search-ring
                          regexp-search-ring)))
    (savehist-mode 1)))

(use-package js
  :demand t
  :config (setq js-indent-level 2))

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

(use-package hl-line
  :demand t
  :config (global-hl-line-mode t))

(use-package comint
  :demand t
  :config
  (progn
    (setq comint-scroll-show-maximum-output nil)
    (remove-hook 'comint-output-filter-functions
                 'comint-postoutput-scroll-to-bottom)))

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

;; pretty-print expression evals.
(use-package pp
  :demand t
  :config
  (progn
    (global-set-key [remap eval-expression] 'pp-eval-expression)
    (global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)))


;; command-as-meta.
(use-package ns-win
  :demand t
  :config
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        ns-function-modifier 'hyper
        ;; TODO: these aren't defined in ns-win -- are they actually used anywhere?
        ;; mac-option-key-is-meta nil
        ;; mac-command-key-is-meta t
        ))


;; speling
(use-package flyspell
  :demand t
  :config
  (progn
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))

;; whitespace.
(use-package indent
  :demand t
  :bind ("<backtab>" . indent-relative)
  :config
  (setq-default indent-tabs-mode nil
                tab-width 2))

(use-package wrap
  :demand t
  :config
  (global-visual-line-mode 1))

;; fix myself.
(use-package dabbrev
  :demand t
  :bind ("C-_" . dabbrev-expand)
  :config (setq abbrev-mode t))

;; vc
(use-package compile
  :demand t
  :config
  (setq compilation-scroll-output t
      compilation-ask-about-save nil
      compilation-save-buffers-predicate '(lambda () nil)))

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

(use-package browse-url
  :demand t
  :config
  (defun browse-url-default-macosx-browser (url &optional new-window)
    "Browse URL in the background. (NEW-WINDOW is ignored)."
    (interactive (browse-url-interactive-arg "URL: "))
    (start-process (concat "open -g" url) nil "open" "-g" url)))

(provide 'init-whilp)
;;; init-whilp ends here
