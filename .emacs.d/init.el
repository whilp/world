;; no bars.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; no splash.
(setq inhibit-startup-message t
  inhibit-startup-echo-area-message t)

;; no prompts.
(setq confirm-nonexistent-file-or-buffer nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; syntax highlighting.
(global-font-lock-mode t)
(transient-mark-mode t)

;; ido matching.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; backups.
(setq backup-directory-alist `(("." . "~/.saves")))

;; pretty-print expression evals.
(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; magit.
(autoload 'magit-status' "magit" nil t)
(global-set-key (kbd "C-x C-g") 'magit-status)

;; org-mode.
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; http://jblevins.org/projects/deft/
(add-to-list 'load-path (expand-file-name "~/.emacs.d/deft"))
(require 'deft)
(setq deft-extension "org")
(setq deft-directory "~/notes")
(setq deft-text-mode 'org-mode)

(set-default-font "Source Code Pro")
