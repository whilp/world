;; environment.
(setq exec-path (append exec-path '("~/bin")))

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
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; pretty-print expression evals.
(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

;; windows.
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
(global-set-key (kbd "C-o") 'other-window)

;; s/meta/c/
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; command-as-meta.
(setq mac-option-key-is-meta nil)       
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq kill-read-only-ok)

;; whitespace.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; wrap.
(global-visual-line-mode 1)

;; easier backward-kill-word.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; packages.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; erc.
(require 'tls)
(require 'erc)

;; magit.
(autoload 'magit-status' "magit" nil t)
(setq magit-git-executable "hub")
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
(setq deft-use-filename-as-title t)

(set-default-font "Source Code Pro-15")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'solarized-dark)
