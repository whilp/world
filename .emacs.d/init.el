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

;; ido matching.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; backups.
(setq backup-directory-alist `(("." . "~/.saves")))

;; pretty-print expression evals.
(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

;; packages.
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

;; magit.
(autoload 'magit-status' "magit" nil t)
(global-set-key (kbd "C-x C-g") 'magit-status)
