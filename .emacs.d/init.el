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

;; whitespace.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; easier backward-kill-word.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

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
