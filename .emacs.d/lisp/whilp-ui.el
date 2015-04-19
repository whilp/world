;;; whilp-ui --- My preferred UI settings.

;;; Commentary:

;;; Opinions.

;;; Code:

(require 'time)

(bind-key "C-x C-c" nil)

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

(setq confirm-nonexistent-file-or-buffer nil
      display-time-default-load-average nil
      display-time-format "%a %Y-%m-%d %H:%M"
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      recenter-positions '(top middle bottom)
      ring-bell-function 'ignore
      visible-bell t)

;; no bars, bells.
(fringe-mode '(nil . -1))
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(winner-mode 1)

;; modeline.
(column-number-mode 0)
(display-battery-mode 0)
(display-time-mode 1)
(line-number-mode 0)
(size-indication-mode 0)
(which-function-mode 0)

;; no prompts.
(fset 'yes-or-no-p 'y-or-n-p)

;; syntax highlighting.
(global-font-lock-mode t)
(transient-mark-mode t)

(provide 'whilp-ui)
;;; whilp-ui ends here
