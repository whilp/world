;;; whilp-ui --- My preferred UI settings.

;;; Commentary:

;;; Opinions.

;;; Code:

(require 'time)

(setq confirm-nonexistent-file-or-buffer nil
      display-time-default-load-average nil
      display-time-format "%a %Y-%m-%d %H:%M"
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      recenter-positions '(top middle bottom)
      ring-bell-function 'ignore
      visible-bell t
      redisplay-dont-pause t)

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
